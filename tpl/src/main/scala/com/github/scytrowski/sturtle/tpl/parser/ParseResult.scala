package com.github.scytrowski.sturtle.tpl.parser

import cats.ApplicativeError
import com.github.scytrowski.sturtle.tpl.parser.ParseResult.Compiler

trait AbstractParseResult

sealed abstract class ParseResult[+T, +E, +A] {
  final def *>[T2 >: T, E2 >: E, B](pb: ParseResult[T2, E2, B]): ParseResult[T2, E2, B] = flatMap(_ => pb)

  def map[B](f: A => B): ParseResult[T, E, B]

  def mapError[E2](f: E => E2): ParseResult[T, E2, A]

  final def flatMap[T2 >: T, E2 >: E, B](f: A => ParseResult[T2, E2, B]): ParseResult[T2, E2, B] = flatMapT(f)

  def flatMapT[T2, E2 >: E, B](f: A => ParseResult[T2, E2, B]): ParseResult[T2, E2, B]

  def flatParseMap[T2 >: T, E2 >: E, B](f: (A, List[T]) => ParseResult[T2, E2, B]): ParseResult[T2, E2, B]

  def drop(n: Int): ParseResult[T, E, A]

  def dropWhile(p: T => Boolean): ParseResult[T, E, A]

  def toEither: Either[E, (A, List[T])]

  final def compile[F[+_]](implicit compiler: Compiler[F, T, E]): F[A] = compiler.compile(this)
}

object ParseResult {
  final case class Success[+T, +A](value: A, remaining: List[T]) extends ParseResult[T, Nothing, A] {
    override def map[B](f: A => B): ParseResult[T, Nothing, B] = copy(value = f(value))

    override def mapError[E2](f: Nothing => E2): ParseResult[T, E2, A] = this

    override def flatMapT[T2, E2 >: Nothing, B](f: A => ParseResult[T2, E2, B]): ParseResult[T2, E2, B] = f(value)

    override def flatParseMap[T2 >: T, E2 >: Nothing, B](f: (A, List[T]) => ParseResult[T2, E2, B]): ParseResult[T2, E2, B] = f(value, remaining)

    override def drop(n: Int): ParseResult[T, Nothing, A] = copy(remaining = remaining.drop(n))

    override def dropWhile(p: T => Boolean): ParseResult[T, Nothing, A] = copy(remaining = remaining.dropWhile(p))

    override def toEither: Either[Nothing, (A, List[T])] = Right(value -> remaining)
  }

  final case class Failure[+E](error: E) extends ParseResult[Nothing, E, Nothing] {
    override def map[B](f: Nothing => B): ParseResult[Nothing, E, B] = this

    override def mapError[E2](f: E => E2): ParseResult[Nothing, E2, Nothing] = Failure(f(error))

    override def flatMapT[T2, E2 >: E, B](f: Nothing => ParseResult[T2, E2, B]): ParseResult[T2, E2, B] = this

    override def flatParseMap[T2 >: Nothing, E2 >: E, B](f: (Nothing, List[Nothing]) => ParseResult[T2, E2, B]): ParseResult[T2, E2, B] = this

    override def drop(n: Int): ParseResult[Nothing, E, Nothing] = this

    override def dropWhile(p: Nothing => Boolean): ParseResult[Nothing, E, Nothing] = this

    override def toEither: Either[E, (Nothing, List[Nothing])] = Left(error)
  }

  trait Compiler[F[_], -T, -E] {
    def compile[A](result: ParseResult[T, E, A]): F[A]
  }

  implicit def forRaiseThrowable[F[_]](implicit raiseThrowable: ApplicativeError[F, Throwable]): Compiler[F, Token, ParseError] = new Compiler[F, Token, ParseError] {
    override def compile[A](result: ParseResult[Token, ParseError, A]): F[A] =
      result match {
        case Success(res, _) => raiseThrowable.pure(res)
        case Failure(error)  => raiseThrowable.raiseError(ParserException(error))
      }
  }
}