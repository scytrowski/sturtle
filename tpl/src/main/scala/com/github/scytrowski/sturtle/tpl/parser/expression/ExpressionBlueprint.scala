package com.github.scytrowski.sturtle.tpl.parser.expression

import com.github.scytrowski.sturtle.tpl.codegen.SyntaxTree.Expression
import com.github.scytrowski.sturtle.tpl.parser.expression.ExpressionToken.{OperatorToken, ValueToken}

final case class ExpressionBlueprint(tokens: List[ExpressionToken], operators: List[OperatorToken]) {
  def add(token: ExpressionToken): ExpressionBlueprint =
    token match {
      case vt: ValueToken => addToken(vt)
      case ot: OperatorToken => addOperator(ot)
    }

  def allTokens: List[ExpressionToken] = tokens ++ operators

  private def addOperator(token: OperatorToken): ExpressionBlueprint = {
    val greaterPrecedenceOperators = operators.takeWhile(_.operator.precedence > token.operator.precedence)
    addTokens(greaterPrecedenceOperators)
      .copy(operators = token +: operators.dropRight(greaterPrecedenceOperators.length))
  }

  private def addToken(token: ExpressionToken): ExpressionBlueprint = addTokens(List(token))

  private def addTokens(newTokens: List[ExpressionToken]): ExpressionBlueprint = copy(tokens = tokens ++ newTokens)
}

object ExpressionBlueprint {
  val empty: ExpressionBlueprint = ExpressionBlueprint(List.empty, List.empty)
}

sealed abstract class ExpressionToken

object ExpressionToken {
  final case class ValueToken(value: Expression) extends ExpressionToken
  final case class OperatorToken(operator: Operator) extends ExpressionToken
}
