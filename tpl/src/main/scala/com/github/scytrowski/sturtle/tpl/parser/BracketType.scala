package com.github.scytrowski.sturtle.tpl.parser

sealed abstract class BracketType(val openToken: Token, val closeToken: Token)

object BracketType {
  case object Round extends BracketType(Token.RoundBracketOpen, Token.RoundBracketClose)
  case object Square extends BracketType(Token.SquareBracketOpen, Token.SquareBracketClose)
  case object Curly extends BracketType(Token.CurlyBracketOpen, Token.CurlyBracketClose)
}
