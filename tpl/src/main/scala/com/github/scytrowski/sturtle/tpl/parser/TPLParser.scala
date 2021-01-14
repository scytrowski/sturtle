package com.github.scytrowski.sturtle.tpl.parser

import cats.data.NonEmptyList
import cats.instances.list._
import cats.syntax.traverse._
import com.github.scytrowski.sturtle.tpl.codegen.Case.Conditional
import com.github.scytrowski.sturtle.tpl.codegen.SyntaxTree.Expression.{Assignment, Name, Static}
import com.github.scytrowski.sturtle.tpl.codegen.{Case, SyntaxTree}
import com.github.scytrowski.sturtle.tpl.interpreter.VoidValue
import com.github.scytrowski.sturtle.tpl.parser.ParseError._

object TPLParser extends TokenParser[SyntaxTree] { gen: SyntaxTreeGenerator =>
  import SyntaxTree._

  override def parse: Parse[Token, ParseError, SyntaxTree] = mainBlock.parse

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
    for {
      _     <- require(Token.If)
      cs    <- branchCases
      csNel <- nel(EmptyBranch)(cs)
    } yield Branch(csNel)
  }

  private val branchCases: P[List[Case]] =
    unfoldWhileDefinedS[Option[CaseType], Case](Some(CaseType.Conditional)) {
      case Some(CaseType.Conditional) =>
        conditionalCase.flatMap { c =>
          head.flatMap {
            case Token.Elif => succeed(Some(CaseType.Conditional) -> c).option
            case Token.Else => succeed(Some(CaseType.Default) -> c).option
            case Token.End => succeed(None -> c).option
            case unexpected => fail(UnexpectedToken(unexpected, List(Token.Elif, Token.Elif, Token.End)))
          }
        }
      case Some(CaseType.Default) =>
        for {
          c <- defaultCase
          _ <- require(Token.End)
        } yield Some(None -> c)
      case None => succeed(None)
    }

  def defaultCase: P[Case] = block.map(Case.Default)

  def conditionalCase: P[Case] =
    for {
      condition <- expression
      _ <- require(Token.Then)
      b <- block
    } yield Conditional(condition, b)

  private def loop(loopType: LoopType): P[SyntaxTree] =
    for {
      _         <- require(Token.While)
      condition <- expression
      _         <- require(Token.Do)
      body      <- block
      _         <- require(Token.End)
    } yield Loop(condition, body)

  private def `return`: P[SyntaxTree] =
    requireHead(Token.Return) *> peekOption.flatMap {
      case Some(Token.Elif | Token.Else | Token.End | Token.EOL) | None => succeed(Return(Static(VoidValue)))
      case _ => expression.map(Return)
    }

  private def functionCallOrAssignment(name: Name): P[SyntaxTree] =
    peek.flatMap {
      case Token.RoundBracketOpen => functionCall(name)
      case Token.EqualsSign => assignment(name)
      case unexpected => fail(UnexpectedToken(unexpected, List(Token.RoundBracketOpen, Token.EqualsSign)))
    }

  private def assignment(name: Name): P[SyntaxTree] =
    for {
      _    <- require(Token.EqualsSign)
      expr <- expression
    } yield Assignment(name, expr)

  private def parameterNameList: P[List[Name]] =
    parameterList(BracketType.Round).flatMap(_
      .map(expressionToName)
      .sequence
    )

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

  private def nel[A](error: => ParseError)(elements: List[A]): P[NonEmptyList[A]] =
    NonEmptyList.fromList(elements) match {
      case Some(l) => succeed(l)
      case None    => fail(error)
    }
}
