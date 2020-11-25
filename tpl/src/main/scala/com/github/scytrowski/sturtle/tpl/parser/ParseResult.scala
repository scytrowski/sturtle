package com.github.scytrowski.sturtle.tpl.parser

sealed abstract class ParseResult[+T, +A] {
  final def *>[T2 >: T, B](pb: ParseResult[T2, B]): ParseResult[T2, B] = flatMap(_ => pb)

  def map[B](f: A => B): ParseResult[T, B]

  def flatMap[T2 >: T, B](f: A => ParseResult[T2, B]): ParseResult[T2, B]

  def flatParseMap[T2 >: T, B](f: (A, List[T]) => ParseResult[T2, B]): ParseResult[T2, B]

  def drop(n: Int): ParseResult[T, A]

  def dropWhile(p: T => Boolean): ParseResult[T, A]

  def toEither: Either[ParseError[T], (A, List[T])]
}

object ParseResult {
  final case class Success[+T, +A](value: A, remaining: List[T]) extends ParseResult[T, A] {
    override def map[B](f: A => B): ParseResult[T, B] = copy(value = f(value))

    override def flatMap[T2, B](f: A => ParseResult[T2, B]): ParseResult[T2, B] = f(value)

    override def flatParseMap[T2 >: T, B](f: (A, List[T]) => ParseResult[T2, B]): ParseResult[T2, B] = f(value, remaining)

    override def drop(n: Int): ParseResult[T, A] = copy(remaining = remaining.drop(n))

    override def dropWhile(p: T => Boolean): ParseResult[T, A] = copy(remaining = remaining.dropWhile(p))

    override def toEither: Either[ParseError[T], (A, List[T])] = Right(value -> remaining)
  }

  final case class Failure[+T](error: ParseError[T]) extends ParseResult[T, Nothing] {
    override def map[B](f: Nothing => B): ParseResult[T, B] = this

    override def flatMap[T2 >: T, B](f: Nothing => ParseResult[T2, B]): ParseResult[T2, B] = this

    override def flatParseMap[T2 >: T, B](f: (Nothing, List[T]) => ParseResult[T2, B]): ParseResult[T2, B] = this

    override def drop(n: Int): ParseResult[T, Nothing] = this

    override def dropWhile(p: T => Boolean): ParseResult[T, Nothing] = this

    override def toEither: Either[ParseError[T], (Nothing, List[T])] = Left(error)
  }
}