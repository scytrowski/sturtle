package com.github.scytrowski.sturtle.es.persistence

import cats.effect.Sync
import com.github.scytrowski.sturtle.es.persistence.db.DoobieEventRepository
import doobie.Transactor
import io.circe.{Decoder, Encoder}

trait Storing[F[_], I, S, E]{
  def eventRepository: EventRepository[F, I, S, E]
}

object Storing {
  def doobie[F[_]: Sync, I: Encoder, S: Encoder: Decoder, E: Encoder: Decoder](xa: Transactor[F]): Storing[F, I, S, E] =
    new Storing[F, I, S, E] {
      override def eventRepository: EventRepository[F, I, S, E] =
        new DoobieEventRepository(xa)
    }
}
