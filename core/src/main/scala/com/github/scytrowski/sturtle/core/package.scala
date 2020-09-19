package com.github.scytrowski.sturtle

import com.github.scytrowski.sturtle.es.{EventSourcing, EventSourcingDescription}

package object core {
  type TurtleEventSourcing[F[_]] = EventSourcing[Turtle, TurtleCommand, TurtleEvent, TurtleQuery, F]
  type TurtleEventSourcingDescription[F[_]] = EventSourcingDescription[Turtle, TurtleCommand, TurtleEvent, TurtleQuery, F]
}
