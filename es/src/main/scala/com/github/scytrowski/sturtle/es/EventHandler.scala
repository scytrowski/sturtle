package com.github.scytrowski.sturtle.es

import cats.Monad
import cats.syntax.flatMap._
import cats.syntax.functor._

trait EventHandler[F[_], S, E] {
  def handle(state: S, event: E): F[S]

  final def handleMany(initialState: S, events: List[E])(implicit F: Monad[F]): F[S] =
    events.foldLeft(F.pure(initialState)) { case (f, event) =>
      F.flatMap(f)(handle(_, event))
    }

  final def andThen(other: EventHandler[F, S, E])(implicit F: Monad[F]): EventHandler[F, S, E] = {
    val ef: (S, E) => F[S] = (s, e) =>
      for {
        s1 <- handle(s, e)
        s2 <- other.handle(s1, e)
      } yield s2
    EventHandler(ef)
  }
}

object EventHandler {
  def apply[F[_], S, E](f: (S, E) => F[S]): EventHandler[F, S, E] = (state: S, event: E) => f(state, event)
}
