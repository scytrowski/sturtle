package com.github.scytrowski.sturtle.core.mock

import cats.effect.concurrent.Ref
import cats.effect.{ContextShift, IO}
import com.github.scytrowski.sturtle.core._
import com.github.scytrowski.sturtle.es.{EventSourcing, EventSourcingDescription, EventSourcingSinks, EventStore, LocalEntityLockManager}

object TestTurtleEventSourcing {
  final case class Data(commands: List[TurtleCommand] = Nil,
                        events: List[TurtleEvent] = Nil,
                        queries: List[TurtleQuery] = Nil)

  def apply(data: Ref[IO, Data])(implicit cs: ContextShift[IO]): IO[TurtleEventSourcing[IO]] = {
    LocalEntityLockManager[IO, String]
      .map(new EventSourcing(description, EventStore.dummy, testSinks(data), _))
  }

  private def testSinks(data: Ref[IO, Data]): TurtleEventSourcingSinks[IO] =
    new EventSourcingSinks[IO, Turtle, TurtleCommand, TurtleEvent, TurtleQuery] {
      override def commandSink(command: TurtleCommand): IO[Unit] =
        data.update(d => d.copy(commands = d.commands :+ command))

      override def eventSink(events: List[TurtleEvent]): IO[Unit] =
        data.update(d => d.copy(events = d.events ++ events))

      override def querySink(query: TurtleQuery): IO[Unit] =
        data.update(d => d.copy(queries = d.queries :+ query))
    }

  private val description: TurtleEventSourcingDescription[IO] =
    EventSourcingDescription(Turtle.initial, TurtleCommand.handler, TurtleEvent.handler, TurtleQuery.handler)
}
