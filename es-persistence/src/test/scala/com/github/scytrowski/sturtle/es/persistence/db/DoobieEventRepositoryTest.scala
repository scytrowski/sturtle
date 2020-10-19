package com.github.scytrowski.sturtle.es.persistence.db

import cats.effect.IO
import cats.instances.list._
import cats.syntax.traverse._
import com.github.scytrowski.sturtle.es.EventDescription
import com.github.scytrowski.sturtle.es.persistence.fixture.DatabaseSpecLike
import com.github.scytrowski.sturtle.es.persistence.mock.TestEventSourcing.{TestEvent, TestState}
import doobie.Transactor
import doobie.implicits._
import io.circe.generic.auto._
import io.circe.{Encoder, parser}

class DoobieEventRepositoryTest extends DatabaseSpecLike {
  "DoobieEventRepository" when {

    "retrieve" when {

      "using the latest snapshot" in {
        val event1 = EventDescription.Event("1", 1, TestEvent("a"))
        val snapshot1 = EventDescription.Snapshot("2", 2, TestState(List("a")))
        val event2 = EventDescription.Event("3", 3, TestEvent("b"))
        val snapshot2 = EventDescription.Snapshot("4", 4, TestState(List("a", "b")))
        val event3 = EventDescription.Event("5", 5, TestEvent("c"))
        val descriptions = List(event1, snapshot1, event2, snapshot2, event3)

        val io = useTransactor { implicit xa =>
          for {
            _      <- insert("test-entity", descriptions)
            repo   = new DoobieEventRepository[IO, String, TestState, TestEvent](xa)
            result <- repo.retrieve("test-entity")
          } yield result
        }

        val result = io.unsafeRunSync()
        result must contain theSameElementsInOrderAs List(snapshot2, event3)
      }

      "no snapshot available" in {
        val descriptions = List(
          EventDescription.Event("1", 1, TestEvent("a")),
          EventDescription.Event("2", 2, TestEvent("b")),
          EventDescription.Event("3", 3, TestEvent("c"))
        )

        val io = useTransactor { implicit xa =>
          for {
            _      <- insert("test-entity", descriptions)
            repo   = new DoobieEventRepository[IO, String, TestState, TestEvent](xa)
            result <- repo.retrieve("test-entity")
          } yield result
        }

        val result = io.unsafeRunSync()
        result must contain theSameElementsInOrderAs descriptions
      }

    }

    "save" should {

      "save encoded event descriptions to the database" in {
        val event1 = EventDescription.Event("1", 1, TestEvent("a"))
        val snapshot1 = EventDescription.Snapshot("2", 2, TestState(List("a")))
        val event2 = EventDescription.Event("3", 3, TestEvent("b"))
        val snapshot2 = EventDescription.Snapshot("4", 4, TestState(List("a", "b")))
        val event3 = EventDescription.Event("5", 5, TestEvent("c"))
        val descriptions = List(event1, snapshot1, event2, snapshot2, event3)

        val io = useTransactor { implicit xa =>
          val repo = new DoobieEventRepository[IO, String, TestState, TestEvent](xa)
          for {
            _         <- repo.save("test-entity", descriptions)
            events    <- selectEvents
            snapshots <- selectSnapshots
          } yield events -> snapshots
        }

        val (events, snapshots) = io.unsafeRunSync()
        events must contain theSameElementsAs List(event1, event2, event3)
        snapshots must contain theSameElementsAs List(snapshot1, snapshot2)
      }

    }

  }

  private def insert(entityId: String, descriptions: List[EventDescription[TestState, TestEvent]])
                    (implicit xa: Transactor[IO]): IO[Unit] = {
    val encodedEntityId = Encoder[String].apply(entityId).noSpaces
    val events = descriptions.collect { case event @ EventDescription.Event(_, _, _) => event }
    val snapshots = descriptions.collect { case snapshot @ EventDescription.Snapshot(_, _, _) => snapshot }
    val eventRows = events.map { ev =>
      val data = Encoder[TestEvent].apply(ev.event).noSpaces
      EventRow(encodedEntityId, ev.id, ev.timestamp, data)
    }
    val snapshotRows = snapshots.map { snap =>
      val data = Encoder[TestState].apply(snap.state).noSpaces
      SnapshotRow(encodedEntityId, snap.id, snap.timestamp, data)
    }
    val connectionIO =
      for {
        _ <- EventStatement.insert.updateMany(eventRows)
        _ <- SnapshotStatement.insert.updateMany(snapshotRows)
      } yield ()
    connectionIO.transact(xa)
  }

  private def selectEvents(implicit xa: Transactor[IO]): IO[List[EventDescription.Event[TestEvent]]] =
    sql"SELECT entityId, id, `timestamp`, data FROM events"
      .query[EventRow]
      .to[List]
      .transact(xa)
      .flatMap { rows =>
        rows.map { row =>
          IO.fromEither(parser.decode[TestEvent](row.data))
            .map(EventDescription.Event(row.id, row.timestamp, _))
        }.sequence
      }

  private def selectSnapshots(implicit xa: Transactor[IO]): IO[List[EventDescription.Snapshot[TestState]]] =
    sql"SELECT entityId, id, `timestamp`, data FROM snapshots"
      .query[SnapshotRow]
      .to[List]
      .transact(xa)
      .flatMap { rows =>
        rows.map { row =>
          IO.fromEither(parser.decode[TestState](row.data))
            .map(EventDescription.Snapshot(row.id, row.timestamp, _))
        }.sequence
      }
}
