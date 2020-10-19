package com.github.scytrowski.sturtle.es.persistence.db

import cats.data.OptionT
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.traverse._
import cats.effect.Sync
import com.github.scytrowski.sturtle.es.EventDescription
import com.github.scytrowski.sturtle.es.persistence.EventRepository
import doobie.ConnectionIO
import doobie.implicits._
import doobie.util.transactor.Transactor
import io.circe.{Decoder, Encoder, parser}

private[persistence] final class DoobieEventRepository[F[_]: Sync, I: Encoder, S: Encoder: Decoder, E: Encoder: Decoder](xa: Transactor[F]) extends EventRepository[F, I, S, E] {
  override def retrieve(entityId: I): F[List[EventDescription[S, E]]] = {
    val entityIdStr = encode(entityId)
    for {
      latestSnapshot  <- retrieveLatestSnapshot(entityIdStr)
      remainingEvents <- retrieveRemainingEvents(entityIdStr, latestSnapshot.map(_.timestamp))
    } yield (latestSnapshot ++ remainingEvents).toList
  }

  override def save(entityId: I, descriptions: List[EventDescription[S, E]]): F[Unit] = {
    val events = descriptions.collect { case event @ EventDescription.Event(_, _, _) => event }
    val snapshots = descriptions.collect { case snapshot @ EventDescription.Snapshot(_, _, _) => snapshot }
    val entityIdStr = encode(entityId)
    val io =
      for {
        _ <- saveEvents(entityIdStr)(events)
        _ <- saveSnapshots(entityIdStr)(snapshots)
      } yield ()
    io.transact(xa)
  }

  private def retrieveLatestSnapshot(entityId: String): F[Option[EventDescription.Snapshot[S]]] =
    OptionT(SnapshotStatement.findLatest(entityId).option.transact(xa))
      .semiflatMap(rowToSnapshot)
      .value

  private def retrieveRemainingEvents(entityId: String, minimumTimestamp: Option[Long]): F[List[EventDescription.Event[E]]] =
    EventStatement
      .findByEntityAndOptTimestamp(entityId, minimumTimestamp)
      .to[List]
      .transact(xa)
      .flatMap(_.map(rowToEvent).sequence)

  private def saveEvents(entityId: String)(events: List[EventDescription.Event[E]]): ConnectionIO[Int] = {
    val rows = events.map(eventToRow(entityId))
    EventStatement.insert.updateMany(rows)
  }

  private def saveSnapshots(entityId: String)(snapshots: List[EventDescription.Snapshot[S]]): ConnectionIO[Int] = {
    val rows = snapshots.map(snapshotToRow(entityId))
    SnapshotStatement.insert.updateMany(rows)
  }

  private def rowToEvent(row: EventRow): F[EventDescription.Event[E]] =
    decode[E](row.data)
      .map(EventDescription.Event(row.id, row.timestamp, _))

  private def eventToRow(entityId: String)(event: EventDescription.Event[E]): EventRow = {
    val data = encode(event.event)
    EventRow(entityId, event.id, event.timestamp, data)
  }

  private def rowToSnapshot(row: SnapshotRow): F[EventDescription.Snapshot[S]] =
    decode[S](row.data)
      .map(EventDescription.Snapshot(row.id, row.timestamp, _))

  private def snapshotToRow(entityId: String)(snapshot: EventDescription.Snapshot[S]): SnapshotRow = {
    val data = encode(snapshot.state)
    SnapshotRow(entityId, snapshot.id, snapshot.timestamp, data)
  }

  private def encode[A: Encoder](a: A): String =
    Encoder[A].apply(a).noSpaces

  private def decode[A: Decoder](json: String): F[A] =
    Sync[F]
      .fromEither(parser.decode[A](json))
}
