package com.github.scytrowski.sturtle.es.persistence

import java.util.UUID
import java.util.concurrent.TimeUnit

import cats.effect.{Clock, Concurrent, Resource}
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.{Eval, Monad}
import com.github.scytrowski.sturtle.es.persistence.PushableStream.Push
import com.github.scytrowski.sturtle.es.{EventDescription, EventStore, EventStoreSession}
import fs2.{Pipe, Stream}

final class PersistentEventStore[F[_]: Concurrent, I, S, E] private[persistence](storing: Storing[F, I, S, E],
                                                                                 snapshotting: Snapshotting[F, S],
                                                                                 batching: Batching[F],
                                                                                 generateId: F[String],
                                                                                 generateTimestamp: F[Long]) extends EventStore[F, I, S, E] {
  override def retrieve(id: I): F[List[EventDescription[S, E]]] =
    storing.eventRepository.retrieve(id)

  override def session(id: I): Resource[F, EventStoreSession[F, S, E]] =
    PushableStream
      .resource(processEvents(id))
      .map(new PersistentEventStoreSession(snapshotting, generateId, generateTimestamp, _))

  private def processEvents(id: I): Pipe[F, EventDescription[S, E], Unit] = stream =>
    stream
      .through(batching.pipe[EventDescription[S, E]])
      .map(_.toList)
      .evalMap(storing.eventRepository.save(id, _))
}

object PersistentEventStore {
  def apply[F[_]: Concurrent: Clock, I, S, E](storing: Storing[F, I, S, E],
                                              snapshotting: Snapshotting[F, S],
                                              batching: Batching[F]): PersistentEventStore[F, I, S, E] =
    new PersistentEventStore(storing, snapshotting, batching, generateId[F], generateTimestamp[F])

  private def generateId[F[_]: Concurrent]: F[String] =
    Concurrent[F].catchNonFatalEval(Eval.always(UUID.randomUUID().toString))

  private def generateTimestamp[F[_]: Clock]: F[Long] =
    Clock[F].monotonic(TimeUnit.MILLISECONDS)
}

private[persistence] final class PersistentEventStoreSession[F[_]: Concurrent, S, E](snapshotting: Snapshotting[F, S],
                                                                generateId: F[String],
                                                                generateTimestamp: F[Long],
                                                                push: Push[F, EventDescription[S, E]]) extends EventStoreSession[F, S, E] {
  override def persist(events: List[E]): F[Unit] =
    Stream
      .iterable(events)
      .evalMap(generateEventDescription)
      .evalMap(push)
      .compile
      .drain

  override def snapshotCandidate(state: S): F[Unit] =
    snapshotting
      .predicate(state)
      .flatMap {
        case true  => generateSnapshotDescription(state).flatMap(push)
        case false => Monad[F].unit
      }

  private def generateEventDescription(event: E): F[EventDescription.Event[E]] =
    for {
      id        <- generateId
      timestamp <- generateTimestamp
    } yield EventDescription.Event(id, timestamp, event)

  private def generateSnapshotDescription(state: S): F[EventDescription.Snapshot[S]] =
    for {
      id        <- generateId
      timestamp <- generateTimestamp
    } yield EventDescription.Snapshot(id, timestamp, state)
}
