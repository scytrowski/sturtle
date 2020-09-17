package com.github.scytrowski.sturtle.core

import cats.effect.{Resource, Sync}

final class LocalTurtleRef[F[_]: Sync](val id: String,
                                       eventSourcing: TurtleEventSourcing[F]) extends TurtleRef[F] {
  override def controller: Resource[F, TurtleController[F]] = {
    val ctrl = LocalTurtleController(id, eventSourcing)
    Resource.liftF(ctrl)
  }
}
