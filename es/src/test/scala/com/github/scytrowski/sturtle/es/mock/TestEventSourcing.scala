package com.github.scytrowski.sturtle.es.mock

import TestEventSourcingDescription._
import cats.effect.{ContextShift, IO}
import cats.effect.concurrent.Ref
import com.github.scytrowski.sturtle.es.mock.TestEventSourcing.Data
import com.github.scytrowski.sturtle.es.{EventSourcedEntity, EventSourcing, EventSourcingSinks, EventStore, LocalEntityLockManager}

object TestEventSourcing {
  type Entity = EventSourcedEntity[IO, TestState, TestCommand, TestEvent, TestQuery]

  final case class Data(commands: List[TestCommand] = Nil,
                        events: List[TestEvent] = Nil,
                        queries: List[TestQuery] = Nil)

  def apply(data: Ref[IO, Data])(implicit cs: ContextShift[IO]): IO[EventSourcing[IO, String, TestState, TestCommand, TestEvent, TestQuery]] =
    LocalEntityLockManager[IO, String]
      .map(new EventSourcing(TestEventSourcingDescription(), EventStore.dummy, new TestEventSourcingSinks(data), _))
}

final class TestEventSourcingSinks(data: Ref[IO, Data]) extends EventSourcingSinks[IO, TestState, TestCommand, TestEvent, TestQuery] {
  override def commandSink(command: TestCommand): IO[Unit] =
    data.update(d => d.copy(commands = d.commands :+ command))

  override def eventSink(events: List[TestEvent]): IO[Unit] =
    data.update(d => d.copy(events = d.events ++ events))

  override def querySink(query: TestQuery): IO[Unit] =
    data.update(d => d.copy(queries = d.queries :+ query))
}
