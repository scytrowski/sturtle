package com.github.scytrowski.sturtle.logging.mock

import cats.Id
import cats.effect.concurrent.Ref
import com.github.scytrowski.sturtle.core.{TurtleCommand, TurtleEvent, TurtleQuery}
import com.github.scytrowski.sturtle.logging.TurtleLogger
import com.github.scytrowski.sturtle.logging.mock.TestTurtleLogger.TestData

final class TestTurtleLogger(data: Ref[Id, TestData]) extends TurtleLogger[Id] {
  override def logCommand(command: TurtleCommand): Id[Unit] =
    data.update(d => d.copy(commands = d.commands :+ command))

  override def logEvents(events: List[TurtleEvent]): Id[Unit] =
    data.update(d => d.copy(events = d.events ++ events))

  override def logQuery(query: TurtleQuery): Id[Unit] =
    data.update(d => d.copy(queries = d.queries :+ query))
}

object TestTurtleLogger {
  final case class TestData(commands: List[TurtleCommand] = Nil,
                            events: List[TurtleEvent] = Nil,
                            queries: List[TurtleQuery] = Nil)
}
