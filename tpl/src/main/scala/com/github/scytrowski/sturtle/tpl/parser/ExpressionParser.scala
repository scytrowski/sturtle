package com.github.scytrowski.sturtle.tpl.parser

import com.github.scytrowski.sturtle.tpl.interpreter.SyntaxTree.Expression
import com.github.scytrowski.sturtle.tpl.parser.ExpressionToken.{OperatorToken, ValueToken}
import com.github.scytrowski.sturtle.tpl.parser.ParseError.WrappedError

object ExpressionParser {
  def parse: Parse[ExpressionToken, Expression] =
    parseRec(List.empty)

  private def parseRec(stack: List[Expression]): Parse[ExpressionToken, Expression] = tokens =>
    tokens.headOption match {
      case Some(ValueToken(value)) => parseRec(value +: stack)(tokens.tail)
      case Some(OperatorToken(operator)) =>
        operator.parse(stack) match {
          case ParseResult.Success(result, remaining) => parseRec(result +: remaining)(tokens.tail)
          case ParseResult.Failure(error) => ParseResult.Failure(WrappedError(error))
        }
      case None if stack.length == 1 => ParseResult.Success(stack.head, List.empty)
      case None => ???
    }
}
