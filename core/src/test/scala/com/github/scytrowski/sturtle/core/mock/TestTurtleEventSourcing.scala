package com.github.scytrowski.sturtle.core.mock

import cats.Id
import cats.effect.IO
import cats.effect.concurrent.Ref
import cats.syntax.flatMap._
import com.github.scytrowski.sturtle.core.{Turtle, TurtleCommand, TurtleEvent, TurtleEventSourcing, TurtleEventSourcingDescription, TurtleQuery}
import com.github.scytrowski.sturtle.es.{CommandHandler, EventSourcing, EventSourcingDescription, QueryHandler}

object TestTurtleEventSourcing {
  final case class TestData(commands: List[TurtleCommand] = Nil,
                            queries: List[TurtleQuery] = Nil)

  def apply(data: Ref[IO, TestData]): TurtleEventSourcing[IO] =
    EventSourcing.basic(testDescription(data))

  private def testDescription(data: Ref[IO, TestData]): TurtleEventSourcingDescription[IO] = {
    val commandHandler = TurtleCommand.handler[IO] andThen CommandHandler((_, c) => data.modify(data => (data.copy(commands = data.commands :+ c)) -> List.empty))
    val queryHandler = testQueryHandler(data)
    EventSourcingDescription(Turtle.initial, commandHandler, TurtleEvent.handler, queryHandler)
  }

  private def testQueryHandler(data: Ref[IO, TestData]): QueryHandler[IO, Turtle, TurtleQuery] =
    new QueryHandler[IO, Turtle, TurtleQuery] {
      override def handle(state: Turtle, query: TurtleQuery): IO[query.Answer] = {
        data.update(data => data.copy(queries = data.queries :+ query)) >>
          TurtleQuery.handler[IO].handle(state, query)
      }
    }
}
