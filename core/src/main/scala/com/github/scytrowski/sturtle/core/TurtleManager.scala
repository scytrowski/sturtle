package com.github.scytrowski.sturtle.core

import cats.Applicative
import cats.data.NonEmptyList
import cats.effect.{Resource, Sync}
import cats.syntax.traverse._
import com.github.scytrowski.sturtle.es.{EventSourcing, EventSourcingDescription}

final class TurtleManager[F[_]: Sync] private(eventSourcing: TurtleEventSourcing[F]) {
  def ref(id: String): TurtleRef[F] = new LocalTurtleRef(id, eventSourcing)
}

object TurtleManager {
  def resource[F[_]: Sync](extensions: List[TurtleExtension]): Resource[F, TurtleManager[F]] = {
    val desc = description[F]
    NonEmptyList
      .of(Resource.pure(EventSourcing.basic(desc)), extensions.map(_.eventSourcing(desc)):_*)
      .sequence
      .map(_.reduceLeft(_ andThen _))
      .map(new TurtleManager(_))
  }

  private def description[F[_]: Applicative]: TurtleEventSourcingDescription[F] =
    EventSourcingDescription(Turtle.initial, TurtleCommand.handler, TurtleEvent.handler, TurtleQuery.handler)
}
