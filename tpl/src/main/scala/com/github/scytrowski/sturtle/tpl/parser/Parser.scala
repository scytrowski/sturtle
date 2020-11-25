package com.github.scytrowski.sturtle.tpl.parser

import cats.{Monad, ~>}

trait Parser[T, +A] {
  def parse: Parse[T, A]

  final def as[B](b: B): Parser[T, B] = map(_ => b)

  final def map[B](f: A => B): Parser[T, B] = Parser(parse(_).map(f))

  final def *>[B](pb: Parser[T, B]): Parser[T, B] = andThen(pb)

  final def andThen[B](pb: Parser[T, B]): Parser[T, B] = flatMap(_ => pb)

  final def flatMap[B](f: A => Parser[T, B]): Parser[T, B] =
    Parser {
      parse(_).flatParseMap { case (a, remaining) => f(a).parse(remaining) }
    }

  final def option: Parser[T, Option[A]] = map(Option(_))

  final def left[B]: Parser[T, Either[A, B]] = map(Left(_))

  final def right[B]: Parser[T, Either[B, A]] = map(Right(_))
}

object Parser {
  def apply[T, A](implicit parser: Parser[T, A]): Parser[T, A] = parser

  def apply[T, A](parseF: Parse[T, A]): Parser[T, A] =
    new Parser[T, A] {
      override val parse: Parse[T, A] = parseF
    }

  def fromParse[T]: Parse[T, *] ~> Parser[T, *] =
    new (Parse[T, *] ~> Parser[T, *]) {
      override def apply[A](fa: Parse[T, A]): Parser[T, A] =
        Parser(fa)
    }

  implicit def monad[T]: Monad[Parser[T, *]] =
    new Monad[Parser[T, *]] {
      override def flatMap[A, B](fa: Parser[T, A])(f: A => Parser[T, B]): Parser[T, B] = fa.flatMap(f)

      override def tailRecM[A, B](a: A)(f: A => Parser[T, Either[A, B]]): Parser[T, B] =
        f(a).flatMap {
          case Left(a) => tailRecM(a)(f)
          case Right(b) => pure(b)
        }

      override def pure[A](x: A): Parser[T, A] = Parser(ParseResult.Success(x, _))
    }
}
