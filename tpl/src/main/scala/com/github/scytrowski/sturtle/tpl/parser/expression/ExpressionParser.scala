package com.github.scytrowski.sturtle.tpl.parser.expression

import com.github.scytrowski.sturtle.tpl.codegen.SyntaxTree.Expression
import com.github.scytrowski.sturtle.tpl.codegen.SyntaxTree.Expression.{Name, Static}
import com.github.scytrowski.sturtle.tpl.interpreter.{BooleanValue, NumberValue, StringValue}
import com.github.scytrowski.sturtle.tpl.parser.ParseError.InvalidBracketConstruction
import com.github.scytrowski.sturtle.tpl.parser.Token.{NameToken, RoundBracketOpen}
import com.github.scytrowski.sturtle.tpl.parser._
import com.github.scytrowski.sturtle.tpl.parser.expression.ExpressionToken.{OperatorToken, ValueToken}

object ExpressionParser extends TokenParser[Expression] { gen: SyntaxTreeGenerator =>
  override def parse: Parse[Token, ParseError, Expression] =
    expressionBlueprint
      .flatMap(parseBlueprint)
      .parse

  private def expressionBlueprint: P[ExpressionBlueprint] =
    expressionElements
      .map(_.foldLeft(ExpressionBlueprint.empty)(_.add(_)))

  private def expressionElements: P[List[ExpressionToken]] =
    unfoldWhileDefinedS(true) { allowPrefixOperators =>
      peekOption.flatMap {
        case Some(Token.NameToken(name)) => nameOrFunctionCall(Name(name)).map(t => false -> Left(t)).option
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
    }.map(_.map {
      case Left(expr) => ValueToken(expr)
      case Right(op)  => OperatorToken(op)
    })

  private def nameOrFunctionCall(name: Name): P[Expression] = {
    requireHead(NameToken(name.value)) *> peekOption.flatMap {
      case Some(RoundBracketOpen) => functionCall(name)
      case _ => succeed[Expression](name)
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

  def parseBlueprint(blueprint: ExpressionBlueprint): P[Expression] =
    liftResult {
      ExpressionTokenParser
        .parse(blueprint.allTokens)
        .mapError(ExpressionError.toParseError)
    }
}
