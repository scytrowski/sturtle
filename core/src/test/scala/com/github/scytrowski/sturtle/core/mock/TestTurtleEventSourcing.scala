package com.github.scytrowski.sturtle.core.mock

import cats.Id
import cats.effect.concurrent.Ref
import com.github.scytrowski.sturtle.core.{Turtle, TurtleCommand, TurtleEvent, TurtleEventSourcing, TurtleEventSourcingDescription}
import com.github.scytrowski.sturtle.es.{CommandHandler, EventSourcing, EventSourcingDescription}

object TestTurtleEventSourcing {
  def apply(commands: Ref[Id, List[TurtleCommand]]): TurtleEventSourcing[Id] =
    EventSourcing.basic(testDescription(commands))

  private def testDescription(commands: Ref[Id, List[TurtleCommand]]): TurtleEventSourcingDescription[Id] = {
    val commandHandler = TurtleCommand.handler[Id] andThen CommandHandler(c => commands.modify(cmds => (cmds :+ c) -> List.empty))
    EventSourcingDescription(Turtle.initial, commandHandler, TurtleEvent.handler)
  }
}
