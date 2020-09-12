package com.github.scytrowski.sturtle.es

import cats.Applicative
import cats.syntax.traverse._

trait CommandHandler[C, E, F[_]] {
  final def handleMany(commands: List[C])(implicit F: Applicative[F]): F[List[E]] =
    commands
      .map(handle)
      .flatSequence

  def handle(command: C): F[List[E]]
}

object CommandHandler {
  def apply[C, E, F[_]](f: C => F[List[E]]): CommandHandler[C, E, F] = (command: C) => f(command)
}