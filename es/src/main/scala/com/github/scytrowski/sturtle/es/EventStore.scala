package com.github.scytrowski.sturtle.es

import cats.Applicative
import cats.syntax.applicative._
import cats.effect.Resource

trait EventStore[F[_], I, S, E] {
  def retrieve(id: I): F[List[RecoveryData[S, E]]]

  def session: Resource[F, EventStoreSession[F, E]]
}

object EventStore {
  def dummy[F[_]: Applicative, I, S, E]: EventStore[F, I, S, E] =
    new EventStore[F, I, S, E] {
      override def retrieve(id: I): F[List[RecoveryData[S, E]]] = List.empty[RecoveryData[S, E]].pure

      override def session: Resource[F, EventStoreSession[F, E]] = Resource.pure(EventStoreSession.dummy)
    }
}

trait EventStoreSession[F[_], E] {
  def persist(events: List[E]): F[Unit]
}

object EventStoreSession {
  def dummy[F[_]: Applicative, E]: EventStoreSession[F, E] =
    (_: List[E]) => ().pure
}
