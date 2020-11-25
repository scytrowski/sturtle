package com.github.scytrowski.sturtle.tpl.parser

sealed abstract class ParseError[+T]

object ParseError {
  final case class UnexpectedToken[+T](actual: T, expected: Option[T] = None) extends ParseError[T]
  final case class InvalidBracketConstruction(bracketType: BracketType) extends ParseError[Nothing]
  case object InvalidName extends ParseError[Nothing]
  case object EmptyBranch extends ParseError[Nothing]
  case object UnexpectedEndOfStream extends ParseError[Nothing]

  sealed abstract class WrappedError[+T] extends ParseError[T] {
    type WT
    def error: ParseError[WT]
  }

  object WrappedError {
    type Aux[T, T2] = WrappedError[T] { type WT = T2 }

    def apply[T, T2](err: ParseError[T2]): WrappedError.Aux[T, T2] =
      new WrappedError[T] {
        override type WT = T2
        override def error: ParseError[WT] = err
      }
  }
}
