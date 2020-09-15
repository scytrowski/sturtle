package com.github.scytrowski.sturtle.es

import cats.Monad
import cats.syntax.functor._
import cats.syntax.flatMap._

trait EventHandler[S, E, F[_]] {
  def handle(state: S, event: E): F[S]

  final def handleMany(initialState: S, events: List[E])(implicit F: Monad[F]): F[S] =
    events.foldLeft(F.pure(initialState)) { case (f, event) =>
      F.flatMap(f)(handle(_, event))
    }

  final def andThen(other: EventHandler[S, E, F])(implicit F: Monad[F]): EventHandler[S, E, F] = {
    val ef: (S, E) => F[S] = (s, e) =>
      for {
        s1 <- handle(s, e)
        s2 <- handle(s1, e)
      } yield s2
    EventHandler(ef)
  }
}

object EventHandler {
  def apply[S, E, F[_]](f: (S, E) => F[S]): EventHandler[S, E, F] = (state: S, event: E) => f(state, event)
}
