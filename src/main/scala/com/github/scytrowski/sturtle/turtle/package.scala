package com.github.scytrowski.sturtle

import com.github.scytrowski.sturtle.es.{EventSourcing, EventSourcingDescription}

package object turtle {
  type TurtleEventSourcing[F[_]] = EventSourcing[Turtle, TurtleCommand, TurtleEvent, F]
  type TurtleEventSourcingDescription[F[_]] = EventSourcingDescription[Turtle, TurtleCommand, TurtleEvent, F]
}
