package com.github.scytrowski.sturtle.es.mock

import cats.effect.{IO, Resource}
import com.github.scytrowski.sturtle.es.{EventStore, EventStoreSession, RecoveryData}
import TestEventSourcingDescription._
import cats.effect.concurrent.Ref
import com.github.scytrowski.sturtle.es.mock.TestEventStore.Data

final class TestEventStore(data: Ref[IO, Data]) extends EventStore[IO, String, TestState, TestEvent] {
  override def retrieve(id: String): IO[List[RecoveryData[TestState, TestEvent]]] =
    data.get.map(_.retrieve)

  override def session: Resource[IO, EventStoreSession[IO, TestEvent]] =
    Resource.pure[IO, TestEventStoreSession](new TestEventStoreSession(data))
}

object TestEventStore {
  final case class Data(retrieve: List[RecoveryData[TestState, TestEvent]] = Nil,
                        persist: List[TestEvent] = Nil)
}

final class TestEventStoreSession(data: Ref[IO, Data]) extends EventStoreSession[IO, TestEvent] {
  override def persist(events: List[TestEvent]): IO[Unit] =
    data.update(d => d.copy(persist = d.persist ++ events))
}
