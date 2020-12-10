package com.github.scytrowski.sturtle.tpl.parser

import cats.Monad
import cats.data.NonEmptyList
import com.github.scytrowski.sturtle.tpl.parser.ParseError.{UnexpectedEndOfStream, UnexpectedToken}
import shapeless.{::, Generic, HList, HNil, Lazy}

trait ParserFactory[T] {
  protected type P[A] = Parser[T, A]
  protected type Error = ParseError[T]

  protected def unfoldWhileDefined[A](pa: P[Option[A]]): P[List[A]] =
    unfoldWhileDefinedS(())(_ => pa.map(_.map(() -> _)))

  protected def unfoldWhileDefinedS[S, A](initial: S)(f: S => P[Option[(S, A)]]): P[List[A]] =
    Monad[P].tailRecM[(S, List[A]), List[A]](initial -> List.empty[A]) { case (state, elements) =>
      f(state).flatMap {
        case Some((newState, el)) => succeed(Left(newState -> (elements :+ el)))
        case None                 => succeed(Right(elements))
      }
    }

  protected val unit: P[Unit] = succeed(())

  protected def succeed[A](value: A): P[A] = lift(t => ParseResult.Success(value, t))

  protected def fail[A](error: Error): P[A] = lift(_ => ParseResult.Failure(error))

  protected def lift[A](parse: Parse[T, A]): P[A] = Parser(parse)

  protected val peek: P[T] = lift { t =>
    1 :: 2 :: HNil
    t.headOption match {
      case Some(head) => ParseResult.Success(head, t)
      case None       => ParseResult.Failure(UnexpectedEndOfStream)
    }
  }

  protected val peekOption: P[Option[T]] = lift(t => ParseResult.Success(t.headOption, t))

  protected val head: P[T] =
    for {
      h <- peek
      _ <- drop(1)
    } yield h

  protected val headOption: P[Option[T]] =
    peekOption.flatMap {
      case some @ Some(_) => drop(1).as(some)
      case None => succeed(None)
    }

  protected def require(first: T, rest: T*): P[Unit] =
    NonEmptyList
      .of(first, rest:_*)
      .map(requireHead)
      .reduceLeft(_ *> _)

  protected def requireHead(token: T): P[Unit] =
    head.flatMap { h =>
      if (h == token)
        succeed(())
      else
        fail(ParseError.UnexpectedToken(h, Some(token)))
    }

  protected val length: P[Int] = lift(t => ParseResult.Success(t.length, t))

  protected val requireEmpty: P[Unit] =
    peekOption.flatMap {
      case Some(unexpected) => fail(UnexpectedToken(unexpected))
      case None => unit
    }

  protected def trim(token: T): P[Unit] = lift(t => ParseResult.Success((), t.dropWhile(_ == token)))

  protected def take(n: Int): P[List[T]] = lift { t =>
    if (t.length >= n)
      ParseResult.Success(t.take(n), t.drop(n))
    else
      ParseResult.Failure(UnexpectedEndOfStream)
  }

  protected def drop(n: Int): P[Unit] = lift(t => ParseResult.Success((), t.drop(n)))

  protected implicit def generic[A, R <: HList](implicit gen: Generic.Aux[A, R], parser: Lazy[P[R]]): P[A] = parser.value.map(gen.from)

  protected implicit def hnil: P[HNil] = succeed(HNil)

  protected implicit def hlist[H, L <: HList](implicit hParser: Lazy[P[H]], lParser: P[L]): P[H :: L] =
    for {
      h <- hParser.value
      l <- lParser
    } yield h :: l
}
