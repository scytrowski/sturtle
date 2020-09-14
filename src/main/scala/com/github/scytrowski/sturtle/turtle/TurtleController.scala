package com.github.scytrowski.sturtle.turtle

import cats.Applicative
import com.github.scytrowski.sturtle.es.{EventSourcing, EventSourcingDescription}
import com.github.scytrowski.sturtle.turtle.TurtleController.TurtleEventSourcing

final class TurtleController[F[_]] private(turtle: Turtle, eventSourcing: TurtleEventSourcing[F])

object TurtleController {
  type TurtleEventSourcing[F[_]] = EventSourcing[Turtle, TurtleCommand, TurtleEvent, F]
  type TurtleEventSourcingDescription[F[_]] = EventSourcingDescription[Turtle, TurtleCommand, TurtleEvent, F]

  def apply[F[_]: Applicative](turtle: Turtle)(esFactory: TurtleEventSourcingDescription[F] => TurtleEventSourcing[F]): TurtleController[F] = {
    val description = EventSourcingDescription(Turtle.initial, TurtleCommand.handler[F], TurtleEvent.handler[F])
    val es = esFactory(description)
    new TurtleController(turtle, es)
  }
}
