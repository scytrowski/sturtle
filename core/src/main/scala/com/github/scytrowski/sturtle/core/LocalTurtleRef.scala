package com.github.scytrowski.sturtle.core

import cats.effect.concurrent.Semaphore
import cats.effect.{Resource, Sync}
import com.github.scytrowski.sturtle.core.syntax.semaphore._

final class LocalTurtleRef[F[_]: Sync](val id: String,
                                       eventSourcing: TurtleEventSourcing[F],
                                       lock: Semaphore[F]) extends TurtleRef[F] {
  override def controller: Resource[F, TurtleController[F]] =
    lock.resource
      .evalMap(_ => LocalTurtleController(id, eventSourcing))
}
