package com.github.scytrowski.sturtle.es

import cats.Monad

trait EventHandler[S, E, F[_]] {
  final def handleMany(initialState: S, events: List[E])(implicit F: Monad[F]): F[S] =
    events.foldLeft(F.pure(initialState)) { case (f, event) =>
      F.flatMap(f)(handle(_, event))
    }

  def handle(state: S, event: E): F[S]
}

object EventHandler {
  def apply[S, E, F[_]](f: (S, E) => F[S]): EventHandler[S, E, F] = (state: S, event: E) => f(state, event)
}
