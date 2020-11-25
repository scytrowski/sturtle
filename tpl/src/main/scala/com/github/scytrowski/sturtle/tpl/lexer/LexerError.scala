package com.github.scytrowski.sturtle.tpl.lexer

sealed abstract class LexerError {
  def lineNumber: Long
}

object LexerError {
  final case class InvalidSymbol(lineNumber: Long, symbol: Character) extends LexerError
  final case class UnclosedString(lineNumber: Long) extends LexerError
  final case class InvalidNumber(lineNumber: Long) extends LexerError
  final case class InvalidInteger(lineNumber: Long) extends LexerError
}
