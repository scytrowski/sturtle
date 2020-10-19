package com.github.scytrowski.sturtle.es

import cats.Applicative
import cats.syntax.applicative._
import cats.effect.Resource

trait EventStore[F[_], I, S, E] {
  def retrieve(id: I): F[List[EventDescription[S, E]]]

  def session(id: I): Resource[F, EventStoreSession[F, S, E]]
}

object EventStore {
  def dummy[F[_]: Applicative, I, S, E]: EventStore[F, I, S, E] =
    new EventStore[F, I, S, E] {
      override def retrieve(id: I): F[List[EventDescription[S, E]]] = List.empty[EventDescription[S, E]].pure

      override def session(id: I): Resource[F, EventStoreSession[F, S, E]] = Resource.pure(EventStoreSession.dummy)
    }
}

trait EventStoreSession[F[_], S, E] {
  def persist(events: List[E]): F[Unit]

  def snapshotCandidate(state: S): F[Unit]
}

object EventStoreSession {
  def dummy[F[_]: Applicative, S, E]: EventStoreSession[F, S, E] =
    new EventStoreSession[F, S, E] {
      override def persist(events: List[E]): F[Unit] = ().pure

      override def snapshotCandidate(state: S): F[Unit] = ().pure
    }
}
