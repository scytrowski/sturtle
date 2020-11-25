package com.github.scytrowski.sturtle.tpl.parser

import cats.data.NonEmptyList
import cats.instances.list._
import cats.syntax.traverse._
import com.github.scytrowski.sturtle.tpl.interpreter.Case.{ConditionalCase, DefaultCase}
import com.github.scytrowski.sturtle.tpl.interpreter.SyntaxTree.Expression.{FunctionCall, Name, Static}
import com.github.scytrowski.sturtle.tpl.interpreter.Value.{BooleanValue, NumberValue, StringValue, VoidValue}
import com.github.scytrowski.sturtle.tpl.interpreter.{Case, SyntaxTree}
import com.github.scytrowski.sturtle.tpl.parser.ParseError.{EmptyBranch, InvalidBracketConstruction, InvalidName, UnexpectedToken, WrappedError}

object TPLParser extends Parser[Token, SyntaxTree] with ParserFactory[Token] with SyntaxTreeGenerator { gen: SyntaxTreeGenerator =>
  import SyntaxTree._

  override def parse: Parse[Token, SyntaxTree] = mainBlock.parse

  private def mainBlock: P[Block] =
    for {
      b <- block
      _ <- requireEmpty
    } yield b

  private def explicitlyDefinedBlock: P[Block] =
    for {
      _ <- require(Token.Block)
      b <- block
      _ <- require(Token.End)
    } yield b

  private def block: P[Block] = unfoldWhileDefined(statement).map(Block)

  private def statement: P[Option[SyntaxTree]] =
    trim(Token.EOL) *> peekOption.flatMap {
      case Some(Token.Block) => explicitlyDefinedBlock.option
      case Some(Token.Function) => functionDefinition.option
      case Some(Token.If) => branch.option
      case Some(Token.While) => loop(LoopType.While).option
      case Some(Token.Repeat) => loop(LoopType.Repeat).option
      case Some(Token.Break) => drop(1) *> succeed(Break).option
      case Some(Token.Return) => `return`.option
      case Some(Token.NameToken(name)) => drop(1) *> functionCallOrAssignment(Name(name)).option
      case _ => succeed(None)
    }

  private def functionDefinition: P[SyntaxTree] =
    for {
      _        <- require(Token.Function)
      funcName <- name
      params   <- parameterNameList
      body     <- block
      _        <- require(Token.End)
    } yield FunctionDefinition(funcName, params, body)

  private def branch: P[SyntaxTree] = {
    def defaultCase: P[Case] = block.map(DefaultCase)

    def conditionalCase: P[Case] =
      for {
        condition <- expression
        _ <- require(Token.Then)
        b <- block
      } yield ConditionalCase(condition, b)

    val cases = unfoldWhileDefinedS[Option[CaseType], Case](Some(CaseType.Conditional)) {
      case Some(CaseType.Conditional) =>
        conditionalCase.flatMap { c =>
          head.flatMap {
            case Token.Elif => succeed(Some(CaseType.Conditional) -> c).option
            case Token.Else => succeed(Some(CaseType.Default) -> c).option
            case Token.End => succeed(None -> c).option
          }
        }
      case Some(CaseType.Default) =>
        for {
          c <- defaultCase
          _ <- require(Token.End)
        } yield Some(None -> c)
      case None => succeed(None)
    }

    for {
      _     <- require(Token.If)
      cs    <- cases
      csNel <- nel(EmptyBranch)(cs)
    } yield Branch(csNel)
  }

  private def loop(loopType: LoopType): P[SyntaxTree] =
    loopType match {
      case LoopType.While =>
        for {
          _         <- require(Token.While)
          condition <- expression
          _         <- require(Token.Do)
          body      <- block
          _         <- require(Token.End)
        } yield Loop(condition, body)
      case LoopType.Repeat =>
        for {
          _     <- require(Token.Repeat)
          times <- expression
          _     <- require(Token.Do)
          body  <- block
          _     <- require(Token.End)
        } yield gen.repeat(times, body)
    }

  private def `return`: P[SyntaxTree] =
    requireHead(Token.Return) *> peekOption.flatMap {
      case Some(Token.Elif | Token.Else | Token.End | Token.EOL) | None => succeed(Return(Static(VoidValue)))
      case _ => expression.map(Return)
    }

  private def functionCallOrAssignment(name: Name): P[SyntaxTree] =
    peek.flatMap {
      case Token.RoundBracketOpen => functionCall(name)
      case Token.Colon => assignment(name)
      case unexpected => fail(UnexpectedToken(unexpected))
    }

  private def functionCall(name: Name): P[SyntaxTree] = parameterList(BracketType.Round).map(FunctionCall(name, _))

  private def assignment(name: Name): P[SyntaxTree] =
    for {
      _    <- require(Token.Colon, Token.EqualsSign)
      expr <- expression
    } yield Assignment(name, expr)

  private def parameterNameList: P[List[Name]] = parameterList(BracketType.Round).flatMap(_.map(expressionToName).sequence)

  private def expression: P[Expression] =
    expressionBlueprint.flatMap { blueprint =>
      ExpressionParser.parse(blueprint.allTokens) match {
        case ParseResult.Success(expr, _) => succeed(expr)
        case ParseResult.Failure(error) => fail(WrappedError(error))
      }
    }

  private def expressionBlueprint: P[ExpressionBlueprint] =
    expressionElements.map(_.foldLeft(ExpressionBlueprint.empty) {
      case (blueprint, Left(value)) => blueprint.addValue(value)
      case (blueprint, Right(operator)) => blueprint.addOperator(operator)
    })

  private def expressionElements: P[List[Either[Expression, Operator]]] =
    unfoldWhileDefinedS(true) { allowPrefixOperators =>
      peekOption.flatMap {
        case Some(Token.NameToken(name)) => drop(1).as(false -> Left(Name(name))).option
        case Some(Token.BooleanToken(value)) => drop(1).as(false -> Left(Static(BooleanValue(value)))).option
        case Some(Token.NumberToken(value)) => drop(1).as(false -> Left(Static(NumberValue(value)))).option
        case Some(Token.StringToken(value)) => drop(1).as(false -> Left(Static(StringValue(value)))).option
        case Some(Token.RoundBracketOpen) => roundBracket.map(false -> Left(_)).option
        case Some(Token.SquareBracketOpen) => squareBracket.map(false -> Left(_)).option
        case Some(Token.CurlyBracketOpen) => curlyBracket.map(false -> Left(_)).option
        case Some(Token.EqualsSign) => drop(1).as(true -> Right(Operator.Equal)).option
        case Some(Token.LessThanSign) => lessOperator.map(true -> Right(_)).option
        case Some(Token.GreaterThanSign) => greaterOperator.map(true -> Right(_)).option
        case Some(Token.Not) if allowPrefixOperators => drop(1).as(false -> Right(Operator.Negate)).option
        case Some(Token.And) => drop(1).as(true -> Right(Operator.And)).option
        case Some(Token.Or) => drop(1).as(true -> Right(Operator.Or)).option
        case Some(Token.Plus) if allowPrefixOperators => drop(1).as(false -> Right(Operator.Plus)).option
        case Some(Token.Plus) => drop(1).as(true -> Right(Operator.Add)).option
        case Some(Token.Minus) if allowPrefixOperators => drop(1).as(false -> Right(Operator.Minus)).option
        case Some(Token.Minus) => drop(1).as(true -> Right(Operator.Subtract)).option
        case Some(Token.Star) => drop(1).as(true -> Right(Operator.Multiply)).option
        case Some(Token.Slash) => drop(1).as(true -> Right(Operator.Divide)).option
        case _ => succeed(None)
      }
    }

  private def roundBracket: P[Expression] =
    parameterList(BracketType.Round).flatMap {
      case expr :: Nil => succeed(expr)
      case first :: second :: Nil => succeed(gen.point(first, second))
      case _ => fail(InvalidBracketConstruction(BracketType.Round))
    }

  private def squareBracket: P[Expression] =
    parameterList(BracketType.Square).flatMap {
      case first :: second :: Nil => succeed(gen.vector(first, second))
      case _ => fail(InvalidBracketConstruction(BracketType.Square))
    }

  private def curlyBracket: P[Expression] =
    parameterList(BracketType.Curly).flatMap {
      case first :: second :: third :: Nil => succeed(gen.color(first, second, third))
      case _ => fail(InvalidBracketConstruction(BracketType.Curly))
    }

  private def lessOperator: P[Operator] =
    require(Token.LessThanSign)
      .flatMap(_ => peek)
      .flatMap {
        case Token.EqualsSign => drop(1).map(_ => Operator.LessOrEqual)
        case Token.GreaterThanSign => drop(1).map(_ => Operator.NotEqual)
        case _ => succeed(Operator.Less)
      }

  private def greaterOperator: P[Operator] =
    require(Token.GreaterThanSign)
      .flatMap(_ => peek)
      .flatMap {
        case Token.EqualsSign => drop(1).map(_ => Operator.GreaterOrEqual)
        case _ => succeed(Operator.Greater)
      }

  private def parameterList(bracketType: BracketType): P[List[Expression]] = {
    val parameters = unfoldWhileDefinedS(true) {
      case true =>
        peek.flatMap {
          case token if token == bracketType.closeToken => drop(1).as(None)
          case _ =>
            expression.flatMap { param =>
              head.flatMap {
                case Token.Comma => succeed(true -> param).option
                case token if token == bracketType.closeToken => succeed(false -> param).option
                case unexpected => fail(UnexpectedToken(unexpected))
              }
            }
        }
      case false => succeed(Option.empty[(Boolean, Expression)])
    }

    require(bracketType.openToken) *> parameters
  }

  private def name: P[Name] =
    head.flatMap {
      case Token.NameToken(name) => succeed(Name(name))
      case unexpected => fail(UnexpectedToken(unexpected))
    }

  private def expressionToName(expr: Expression): P[Name] =
    expr match {
      case name: Name => succeed(name)
      case _ => fail(InvalidName)
    }

  private def nel[A](error: => ParseError[Token])(elements: List[A]): P[NonEmptyList[A]] =
    NonEmptyList.fromList(elements) match {
      case Some(l) => succeed(l)
      case None    => fail(error)
    }
}
