package com.github.scytrowski.sturtle.es.mock

import cats.effect.IO
import com.github.scytrowski.sturtle.es.{EventSourcingDescription, Query, QueryHandler}

object TestEventSourcingDescription {
  def apply(): EventSourcingDescription[IO, TestState, TestCommand, TestEvent, TestQuery] =
    EventSourcingDescription(
      TestState(Nil),
      (_, c) => IO.pure(List(TestEvent(c.data))),
      (s, e) => IO.pure(s.copy(elements = s.elements :+ e.data)),
      new QueryHandler[IO, TestState, TestQuery] {
        override def handle(state: TestState, query: TestQuery): IO[query.Answer] = IO.pure(query.extractAnswer(state))
      }
    )

  final case class TestState(elements: List[String])
  final case class TestCommand(data: String)
  final case class TestEvent(data: String)
  sealed abstract class TestQuery extends Query[TestState]
  case object TestQuery extends TestQuery {
    override type Answer = List[String]

    override def extractAnswer(state: TestState): List[String] = state.elements
  }
}
