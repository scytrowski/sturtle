package com.github.scytrowski.sturtle.tpl.parser.expression

import com.github.scytrowski.sturtle.tpl.codegen.SyntaxTree.Expression
import com.github.scytrowski.sturtle.tpl.parser.ParseError

sealed abstract class ExpressionError

object ExpressionError {
  def toParseError(err: ExpressionError): ParseError = err match {
    case UnexpectedEndOfStream => ParseError.UnexpectedEndOfStream
    case _ => ParseError.IllegalExpression(err)
  }

  final case class IllegalStackState(expressionStack: List[Expression]) extends ExpressionError
  case object UnexpectedEndOfStream extends ExpressionError
}
