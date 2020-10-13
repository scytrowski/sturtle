package com.github.scytrowski.sturtle.es

import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.traverse._
import cats.{Applicative, Monad}

trait CommandHandler[F[_], S, C, E] { self =>
  def handle(state: S, command: C): F[List[E]]

  final def handleMany(state: S, commands: List[C])(implicit F: Applicative[F]): F[List[E]] =
    commands
      .map(handle(state, _))
      .flatSequence

  def andThen(other: CommandHandler[F, S, C, E])(implicit F: Monad[F]): CommandHandler[F, S, C, E] = {
    val cf: (S, C) => F[List[E]] = {
      case (s, c) =>
        for {
          events1 <- handle(s, c)
          events2 <- other.handle(s, c)
        } yield events1 ++ events2
    }
    CommandHandler(cf)
  }
}

object CommandHandler {
  def apply[F[_], S, C, E](f: (S, C) => F[List[E]]): CommandHandler[F, S, C, E] = (state: S, command: C) => f(state, command)
}