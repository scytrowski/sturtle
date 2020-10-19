package com.github.scytrowski.sturtle.es.persistence.mock

import cats.effect.IO
import cats.effect.concurrent.Ref
import com.github.scytrowski.sturtle.es.EventDescription
import com.github.scytrowski.sturtle.es.persistence.{EventRepository, Storing}
import com.github.scytrowski.sturtle.es.persistence.mock.TestEventSourcing.{TestEvent, TestState}
import TestStoring.Data

final class TestStoring(data: Ref[IO, Data]) extends Storing[IO, String, TestState, TestEvent] {
  override def eventRepository: EventRepository[IO, String, TestState, TestEvent] =
    new EventRepository[IO, String, TestState, TestEvent] {
      override def retrieve(entityId: String): IO[List[EventDescription[TestState, TestEvent]]] =
        data.get.map(_.retrieve)

      override def save(entityId: String, descriptions: List[EventDescription[TestState, TestEvent]]): IO[Unit] =
        data.update(d => d.copy(save = d.save :+ (entityId -> descriptions)))
    }
}

object TestStoring {
  final case class Data(retrieve: List[EventDescription[TestState, TestEvent]] = Nil,
                        save: List[(String, List[EventDescription[TestState, TestEvent]])] = Nil)
}