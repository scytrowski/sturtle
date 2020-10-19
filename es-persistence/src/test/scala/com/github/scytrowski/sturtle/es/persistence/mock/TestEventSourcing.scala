package com.github.scytrowski.sturtle.es.persistence.mock

object TestEventSourcing {
  final case class TestState(data: List[String])

  final case class TestEvent(element: String)
}
