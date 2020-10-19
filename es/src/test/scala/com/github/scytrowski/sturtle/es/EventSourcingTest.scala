package com.github.scytrowski.sturtle.es

import cats.effect.IO
import cats.effect.concurrent.Ref
import com.github.scytrowski.sturtle.es.EventDescription.Snapshot
import com.github.scytrowski.sturtle.es.fixture.EffectSpecLike
import com.github.scytrowski.sturtle.es.mock.TestEventSourcingDescription.{TestCommand, TestEvent, TestQuery, TestState}
import com.github.scytrowski.sturtle.es.mock.{TestEventSourcingDescription, TestEventStore}

class EventSourcingTest extends EffectSpecLike {
  "EventSourcing" when {

    "entity" should {

      "perform recovery" in {
        val snapshot = List("a", "b", "c")
        val io =
          for {
            storeData     <- ref(TestEventStore.Data(retrieve = List(Snapshot("1", 1, TestState(snapshot)))))
            eventSourcing <- testEventSourcing(storeData)
            result        <- eventSourcing.entity("test-entity").use(_.execute(TestQuery))
          } yield result

        val result = io.unsafeRunSync()
        result must contain theSameElementsInOrderAs snapshot
      }

    }

  }

  private def testEventSourcing(storeData: Ref[IO, TestEventStore.Data]): IO[EventSourcing[IO, String, TestState, TestCommand, TestEvent, TestQuery]] =
    LocalEntityLockManager[IO, String]
      .map(new EventSourcing(
        TestEventSourcingDescription(),
        new TestEventStore(storeData),
        EventSourcingSinks.ignore,
        _
      ))
}
