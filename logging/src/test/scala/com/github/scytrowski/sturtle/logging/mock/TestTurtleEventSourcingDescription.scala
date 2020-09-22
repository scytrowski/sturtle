package com.github.scytrowski.sturtle.logging.mock

import cats.Applicative
import com.github.scytrowski.sturtle.core.{Turtle, TurtleCommand, TurtleEvent, TurtleEventSourcingDescription, TurtleQuery}
import com.github.scytrowski.sturtle.es.EventSourcingDescription

object TestTurtleEventSourcingDescription {
  def apply[F[_]: Applicative]: TurtleEventSourcingDescription[F] =
    EventSourcingDescription(Turtle.initial, TurtleCommand.handler[F], TurtleEvent.handler[F], TurtleQuery.handler[F])
}
