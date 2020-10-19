package com.github.scytrowski.sturtle.es.persistence

import cats.effect.IO
import com.github.scytrowski.sturtle.es.EventDescription
import com.github.scytrowski.sturtle.es.persistence.fixture.EffectSpecLike
import com.github.scytrowski.sturtle.es.persistence.mock.TestStoring
import com.github.scytrowski.sturtle.es.persistence.mock.TestEventSourcing.{TestEvent, TestState}
import org.scalatest.{Inside, LoneElement}

class PersistentEventStoreTest extends EffectSpecLike with LoneElement with Inside {
  "PersistentEventStore" when {

    "retrieve" should {

      "return descriptions from repository" in {
        val descriptions = List(
          EventDescription.Event("1", 1, TestEvent("a")),
          EventDescription.Event("2", 2, TestEvent("b"))
        )

        val io =
          for {
            data  <- ref(TestStoring.Data(retrieve = descriptions))
            store = new PersistentEventStore[IO, String, TestState, TestEvent](
              new TestStoring(data),
              Snapshotting.always,
              Batching.fixedSize(1),
              IO.pure("1"),
              IO.pure(1)
            )
            result <- store.retrieve("abc")
          } yield result

        val result = io.unsafeRunSync()
        result must contain theSameElementsInOrderAs descriptions
      }

    }

    "session" should {

      "open persistent store session and save single batch" in {
        val event1 = TestEvent("a")
        val snapshot = TestState(List("a"))
        val event2 = TestEvent("b")

        val io =
          for {
            data  <- ref(TestStoring.Data())
            idGen <- generator(List("a", "b", "c"), "unexpected")
            timestampGen <- generator(List[Long](1, 2, 3), -1.toLong)
            store = new PersistentEventStore[IO, String, TestState, TestEvent](
              new TestStoring(data),
              Snapshotting.always,
              Batching.fixedSize(3),
              idGen,
              timestampGen
            )
            _ <- store.session("abc").use { session =>
              session.persist(List(event1)) *>
                session.snapshotCandidate(snapshot) *>
                session.persist(List(event2))
            }
            d <- data.get
          } yield d

        val result = io.unsafeRunSync()
        result.save.loneElement mustBe ("abc" -> List(
          EventDescription.Event("a", 1, event1),
          EventDescription.Snapshot("b", 2, snapshot),
          EventDescription.Event("c", 3, event2)
        ))
      }

      "open persistent store session and save multiple batches" in {
        val event1 = TestEvent("a")
        val snapshot = TestState(List("a"))
        val event2 = TestEvent("b")

        val io =
          for {
            data  <- ref(TestStoring.Data())
            idGen <- generator(List("a", "b", "c"), "unexpected")
            timestampGen <- generator(List[Long](1, 2, 3), -1.toLong)
            store = new PersistentEventStore[IO, String, TestState, TestEvent](
              new TestStoring(data),
              Snapshotting.always,
              Batching.fixedSize(2),
              idGen,
              timestampGen
            )
            _ <- store.session("abc").use { session =>
              session.persist(List(event1)) *>
                session.snapshotCandidate(snapshot) *>
                session.persist(List(event2))
            }
            d <- data.get
          } yield d

        val result = io.unsafeRunSync()
        inside(result.save) { case List(firstBatch, secondBatch) =>
          firstBatch mustBe ("abc" -> List(
            EventDescription.Event("a", 1, event1),
            EventDescription.Snapshot("b", 2, snapshot)
          ))

          secondBatch mustBe ("abc" -> List(EventDescription.Event("c", 3, event2)))
        }
      }

    }

  }
}
