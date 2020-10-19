package com.github.scytrowski.sturtle.es.mock

import cats.effect.{IO, Resource}
import com.github.scytrowski.sturtle.es.{EventStore, EventStoreSession, EventDescription}
import TestEventSourcingDescription._
import cats.effect.concurrent.Ref
import com.github.scytrowski.sturtle.es.mock.TestEventStore.Data

final class TestEventStore(data: Ref[IO, Data]) extends EventStore[IO, String, TestState, TestEvent] {
  override def retrieve(id: String): IO[List[EventDescription[TestState, TestEvent]]] =
    data.get.map(_.retrieve)

  override def session(id: String): Resource[IO, EventStoreSession[IO, TestState, TestEvent]] =
    Resource.pure[IO, TestEventStoreSession](new TestEventStoreSession(data))
}

object TestEventStore {
  final case class Data(retrieve: List[EventDescription[TestState, TestEvent]] = Nil,
                        persist: List[TestEvent] = Nil,
                        snapshotCandidate: List[TestState] = Nil)
}

final class TestEventStoreSession(data: Ref[IO, Data]) extends EventStoreSession[IO, TestState, TestEvent] {
  override def persist(events: List[TestEvent]): IO[Unit] =
    data.update(d => d.copy(persist = d.persist ++ events))

  override def snapshotCandidate(state: TestState): IO[Unit] =
    data.update(d => d.copy(snapshotCandidate = d.snapshotCandidate :+ state))
}
