package com.github.scytrowski.sturtle.es

import cats.Applicative
import cats.syntax.applicative._

trait EventSourcingSinks[F[_], S, C, E, Q <: Query[S]] {
  def commandSink(command: C): F[Unit]

  def eventSink(events: List[E]): F[Unit]

  def querySink(query: Q): F[Unit]
}

object EventSourcingSinks {
  def ignore[F[_]: Applicative, S, C, E, Q <: Query[S]]: EventSourcingSinks[F, S, C, E, Q] =
    new EventSourcingSinks[F, S, C, E, Q] {
      override def commandSink(command: C): F[Unit] = ().pure

      override def eventSink(events: List[E]): F[Unit] = ().pure

      override def querySink(query: Q): F[Unit] = ().pure
    }
}
