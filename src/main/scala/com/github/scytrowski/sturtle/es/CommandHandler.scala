package com.github.scytrowski.sturtle.es

import cats.Applicative

trait CommandHandler[C, E, F[_]] {
  final def handleMany(commands: List[C])(implicit F: Applicative[F]): F[List[E]] =
    commands
      .map(handle)
      .flatSequence

  def handle(command: C): F[List[E]]
}
