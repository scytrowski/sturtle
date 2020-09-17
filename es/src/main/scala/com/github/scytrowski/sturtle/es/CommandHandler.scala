package com.github.scytrowski.sturtle.es

import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.traverse._
import cats.{Applicative, Monad}

trait CommandHandler[C, E, F[_]] { self =>
  def handle(command: C): F[List[E]]

  final def handleMany(commands: List[C])(implicit F: Applicative[F]): F[List[E]] =
    commands
      .map(handle)
      .flatSequence

  def andThen(other: CommandHandler[C, E, F])(implicit F: Monad[F]): CommandHandler[C, E, F] = {
    val cf: C => F[List[E]] = c =>
      for {
        events1 <- handle(c)
        events2 <- other.handle(c)
      } yield events1 ++ events2
    CommandHandler(cf)
  }
}

object CommandHandler {
  def apply[C, E, F[_]](f: C => F[List[E]]): CommandHandler[C, E, F] = (command: C) => f(command)
}