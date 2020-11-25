package com.github.scytrowski.sturtle.tpl.parser

import com.github.scytrowski.sturtle.tpl.interpreter.SyntaxTree.Expression
import com.github.scytrowski.sturtle.tpl.parser.ExpressionToken.{OperatorToken, ValueToken}

final case class ExpressionBlueprint(tokens: List[ExpressionToken], operators: List[Operator]) {
  def addValue(value: Expression): ExpressionBlueprint = addToken(ValueToken(value))

  def addOperator(operator: Operator): ExpressionBlueprint = {
    val greaterPrecedenceOperators = operators.takeWhile(_.precedence > operator.precedence)
    greaterPrecedenceOperators
      .map(OperatorToken)
      .foldLeft(this)(_.addToken(_))
      .copy(operators = operator +: operators.dropRight(greaterPrecedenceOperators.length))
  }

  def allTokens: List[ExpressionToken] = tokens ++ operators.map(OperatorToken)

  private def addToken(token: ExpressionToken): ExpressionBlueprint =
    copy(tokens = tokens :+ token)
}

object ExpressionBlueprint {
  val empty: ExpressionBlueprint = ExpressionBlueprint(List.empty, List.empty)
}

sealed abstract class ExpressionToken

object ExpressionToken {
  final case class ValueToken(value: Expression) extends ExpressionToken
  final case class OperatorToken(operator: Operator) extends ExpressionToken
}
