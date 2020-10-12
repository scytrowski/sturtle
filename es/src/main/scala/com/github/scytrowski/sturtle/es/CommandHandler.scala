package com.github.scytrowski.sturtle.es

import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.traverse._
import cats.{Applicative, Monad}

trait CommandHandler[S, C, E, F[_]] { self =>
  def handle(state: S, command: C): F[List[E]]

  final def handleMany(state: S, commands: List[C])(implicit F: Applicative[F]): F[List[E]] =
    commands
      .map(handle(state, _))
      .flatSequence

  def andThen(other: CommandHandler[S, C, E, F])(implicit F: Monad[F]): CommandHandler[S, C, E, F] = {
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
  def apply[S, C, E, F[_]](f: (S, C) => F[List[E]]): CommandHandler[S, C, E, F] = (state: S, command: C) => f(state, command)
}