package com.github.scytrowski.sturtle

import com.github.scytrowski.sturtle.es.{EventSourcedEntity, EventSourcing, EventSourcingDescription, EventSourcingSinks}

package object core {
  type TurtleEventSourcing[F[_]] = EventSourcing[F, String, Turtle, TurtleCommand, TurtleEvent, TurtleQuery]
  type TurtleEventSourcingDescription[F[_]] = EventSourcingDescription[F, Turtle, TurtleCommand, TurtleEvent, TurtleQuery]
  type TurtleEventSourcedEntity[F[_]] = EventSourcedEntity[F, Turtle, TurtleCommand, TurtleEvent, TurtleQuery]
  type TurtleEventSourcingSinks[F[_]] = EventSourcingSinks[F, Turtle, TurtleCommand, TurtleEvent, TurtleQuery]
}
