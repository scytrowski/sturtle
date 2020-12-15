package com.github.scytrowski.sturtle.tpl.parser

import com.github.scytrowski.sturtle.tpl.types.Complex

abstract class Token

object Token {
  final case class NameToken(value: String) extends Token
  final case class BooleanToken(value: Boolean) extends Token
  final case class NumberToken(value: Complex) extends Token
  final case class StringToken(value: String) extends Token
  case object Block extends Token
  case object Function extends Token
  case object If extends Token
  case object Elif extends Token
  case object Else extends Token
  case object Then extends Token
  case object While extends Token
  case object Repeat extends Token
  case object Do extends Token
  case object Break extends Token
  case object Return extends Token
  case object End extends Token
  case object Not extends Token
  case object And extends Token
  case object Or extends Token
  case object RoundBracketOpen extends Token
  case object RoundBracketClose extends Token
  case object SquareBracketOpen extends Token
  case object SquareBracketClose extends Token
  case object CurlyBracketOpen extends Token
  case object CurlyBracketClose extends Token
  case object Plus extends Token
  case object Minus extends Token
  case object Star extends Token
  case object Slash extends Token
  case object EqualsSign extends Token
  case object LessThanSign extends Token
  case object GreaterThanSign extends Token
  case object Colon extends Token
  case object Comma extends Token
  case object EOL extends Token
}
