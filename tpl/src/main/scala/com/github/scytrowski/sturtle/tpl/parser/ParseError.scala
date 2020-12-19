package com.github.scytrowski.sturtle.tpl.parser

import com.github.scytrowski.sturtle.tpl.parser.expression.ExpressionError

sealed abstract class ParseError

object ParseError {
  final case class UnexpectedToken(actual: Token, expected: Option[Token] = None) extends ParseError
  final case class InvalidBracketConstruction(bracketType: BracketType) extends ParseError
  case object InvalidName extends ParseError
  case object EmptyBranch extends ParseError
  case object UnexpectedEndOfStream extends ParseError
  final case class IllegalExpression(error: ExpressionError) extends ParseError
}
