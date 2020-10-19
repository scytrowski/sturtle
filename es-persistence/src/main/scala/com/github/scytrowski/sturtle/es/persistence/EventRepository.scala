package com.github.scytrowski.sturtle.es.persistence

import cats.data.OptionT
import cats.effect.Sync
import cats.instances.list._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.traverse._
import com.github.scytrowski.sturtle.es.EventDescription
import com.github.scytrowski.sturtle.es.persistence.db.{EventRow, EventStatement, SnapshotRow, SnapshotStatement}
import doobie.ConnectionIO
import doobie.implicits._
import doobie.util.transactor.Transactor
import io.circe.{Decoder, Encoder, parser}

trait EventRepository[F[_], I, S, E] {
  def retrieve(entityId: I): F[List[EventDescription[S, E]]]

  def save(entityId: I, descriptions: List[EventDescription[S, E]]): F[Unit]
}
