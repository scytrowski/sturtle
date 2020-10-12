package com.github.scytrowski.sturtle.core.mock

import cats.Id
import cats.effect.concurrent.Ref
import cats.syntax.flatMap._
import com.github.scytrowski.sturtle.core.{Turtle, TurtleCommand, TurtleEvent, TurtleEventSourcing, TurtleEventSourcingDescription, TurtleQuery}
import com.github.scytrowski.sturtle.es.{CommandHandler, EventSourcing, EventSourcingDescription, QueryHandler}

object TestTurtleEventSourcing {
  final case class TestData(commands: List[TurtleCommand] = Nil,
                            queries: List[TurtleQuery] = Nil)

  def apply(data: Ref[Id, TestData]): TurtleEventSourcing[Id] =
    EventSourcing.basic(testDescription(data))

  private def testDescription(data: Ref[Id, TestData]): TurtleEventSourcingDescription[Id] = {
    val commandHandler = TurtleCommand.handler[Id] andThen CommandHandler((_, c) => data.modify(data => (data.copy(commands = data.commands :+ c)) -> List.empty))
    val queryHandler = testQueryHandler(data)
    EventSourcingDescription(Turtle.initial, commandHandler, TurtleEvent.handler, queryHandler)
  }

  private def testQueryHandler(data: Ref[Id, TestData]): QueryHandler[Turtle, TurtleQuery, Id] =
    new QueryHandler[Turtle, TurtleQuery, Id] {
      override def handle(state: Turtle, query: TurtleQuery): Id[query.Answer] = {
        data.update(data => data.copy(queries = data.queries :+ query)) >>
          TurtleQuery.handler[Id].handle(state, query)
      }
    }
}
