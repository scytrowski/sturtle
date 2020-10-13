package com.github.scytrowski.sturtle

import com.github.scytrowski.sturtle.es.{EventSourcing, EventSourcingDescription}

package object core {
  type TurtleEventSourcing[F[_]] = EventSourcing[F, Turtle, TurtleCommand, TurtleEvent, TurtleQuery]
  type TurtleEventSourcingDescription[F[_]] = EventSourcingDescription[F, Turtle, TurtleCommand, TurtleEvent, TurtleQuery]
}
