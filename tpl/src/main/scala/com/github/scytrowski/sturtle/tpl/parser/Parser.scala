package com.github.scytrowski.sturtle.tpl.parser

import cats.{Monad, ~>}

trait Parser[T, +E, +A] {
  def parse: Parse[T, E, A]

  final def as[B](b: B): Parser[T, E, B] = map(_ => b)

  final def map[B](f: A => B): Parser[T, E, B] = Parser(parse(_).map(f))

  final def *>[E2 >: E, B](pb: Parser[T, E2, B]): Parser[T, E2, B] = andThen(pb)

  final def andThen[E2 >: E, B](pb: Parser[T, E2, B]): Parser[T, E2, B] = flatMap(_ => pb)

  final def flatMap[E2 >: E, B](f: A => Parser[T, E2, B]): Parser[T, E2, B] =
    Parser {
      parse(_).flatParseMap { case (a, remaining) => f(a).parse(remaining) }
    }

  final def option: Parser[T, E, Option[A]] = map(Option(_))

  final def left[B]: Parser[T, E, Either[A, B]] = map(Left(_))

  final def right[B]: Parser[T, E, Either[B, A]] = map(Right(_))
}

object Parser {
  def apply[T, E, A](implicit parser: Parser[T, E, A]): Parser[T, E, A] = parser

  def apply[T, E, A](parseF: Parse[T, E, A]): Parser[T, E, A] =
    new Parser[T, E, A] {
      override val parse: Parse[T, E, A] = parseF
    }

  def proxy[T, E, A](f: List[T] => Parser[T, E, A]): Parser[T, E, A] =
    Parser[T, E, A]((tokens: List[T]) => f(tokens).parse(tokens))

  def fromParse[T, E]: Parse[T, E, *] ~> Parser[T, E, *] =
    new (Parse[T, E, *] ~> Parser[T, E, *]) {
      override def apply[A](fa: Parse[T, E, A]): Parser[T, E, A] =
        Parser(fa)
    }

  implicit def monad[T, E]: Monad[Parser[T, E, *]] =
    new Monad[Parser[T, E, *]] {
      override def flatMap[A, B](fa: Parser[T, E, A])(f: A => Parser[T, E, B]): Parser[T, E, B] = fa.flatMap(f)

      override def tailRecM[A, B](a: A)(f: A => Parser[T, E, Either[A, B]]): Parser[T, E, B] =
        f(a).flatMap {
          case Left(a) => tailRecM(a)(f)
          case Right(b) => pure(b)
        }

      override def pure[A](x: A): Parser[T, E, A] = Parser(ParseResult.Success(x, _))
    }
}
