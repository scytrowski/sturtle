package com.github.scytrowski.sturtle.core.syntax

import cats.Functor
import cats.effect.Resource
import cats.effect.concurrent.Semaphore

object semaphore {
  implicit class SemaphoreAsResource[F[_]: Functor](semaphore: Semaphore[F]) {
    def resource: Resource[F, Unit] =
      Resource.make(semaphore.acquire)(_ => semaphore.release)
  }
}
