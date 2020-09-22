package com.github.scytrowski.sturtle.logging

import com.github.scytrowski.sturtle.core.{TurtleCommand, TurtleEvent, TurtleQuery}

trait TurtleLogger[F[_]] {
  def logCommand(command: TurtleCommand): F[Unit]
  def logEvents(events: List[TurtleEvent]): F[Unit]
  def logQuery(query: TurtleQuery): F[Unit]
}
