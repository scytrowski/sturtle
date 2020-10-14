package com.github.scytrowski.sturtle.core

import cats.Monad
import cats.effect.Resource

trait TurtleRef[F[_]] {
  def id: String

  def controller: Resource[F, TurtleController[F]]
}

private[core] final class LocalTurtleRef[F[_]: Monad](val id: String,
                                                      eventSourcing: TurtleEventSourcing[F]) extends TurtleRef[F] {
  override def controller: Resource[F, TurtleController[F]] =
    eventSourcing
      .entity(id)
      .map(new LocalTurtleController(_))
}