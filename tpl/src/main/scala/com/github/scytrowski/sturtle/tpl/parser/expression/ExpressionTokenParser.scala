package com.github.scytrowski.sturtle.tpl.parser.expression

import com.github.scytrowski.sturtle.tpl.codegen.SyntaxTree.Expression
import com.github.scytrowski.sturtle.tpl.parser.expression.ExpressionError.IllegalStackState
import com.github.scytrowski.sturtle.tpl.parser.expression.ExpressionToken.{OperatorToken, ValueToken}
import com.github.scytrowski.sturtle.tpl.parser.{Parse, ParseResult, Parser, ParserFactory}

object ExpressionTokenParser extends Parser[ExpressionToken, ExpressionError, Expression] with ParserFactory[ExpressionToken, ExpressionError] {
  override def parse: Parse[ExpressionToken, ExpressionError, Expression] =
    foldLeftTokens[List[Expression]](List.empty) { case (stack, token) =>
      token match {
        case ValueToken(value) => succeed(value +: stack)
        case OperatorToken(operator) =>
          operator.parse(stack) match {
            case ParseResult.Success(result, remaining) => succeed(result +: remaining)
            case ParseResult.Failure(error) => fail(error)
          }
      }
    }.flatMap {
      case result :: Nil => drain *> succeed(result)
      case illegal => fail(IllegalStackState(illegal))
    }.parse
}
