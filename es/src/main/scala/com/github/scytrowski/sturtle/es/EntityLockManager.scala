package com.github.scytrowski.sturtle.es

import cats.Applicative
import cats.effect.concurrent.{MVar, MVar2, Semaphore}
import cats.effect.{Concurrent, Resource}
import cats.syntax.functor._
import com.github.scytrowski.sturtle.es.syntax.semaphore._

trait EntityLockManager[F[_], I] {
  def retrieve(id: I): Resource[F, Unit]
}

final class LocalEntityLockManager[F[_]: Concurrent, I] private(locks: MVar2[F, Map[I, Semaphore[F]]]) extends EntityLockManager[F, I] {
  def retrieve(id: I): Resource[F, Unit] =
    Resource.liftF {
      locks.modify { locks =>
        locks.get(id) match {
          case Some(lock) => Applicative[F].pure(locks -> lock)
          case None => createLock.map(lock => (locks + (id -> lock)) -> lock)
        }
      }
    }.flatMap(_.resource)

  private def createLock: F[Semaphore[F]] = Semaphore[F](1)
}

object LocalEntityLockManager {
  def apply[F[_]: Concurrent, I]: F[LocalEntityLockManager[F, I]] =
    MVar
      .of(Map.empty[I, Semaphore[F]])
      .map(new LocalEntityLockManager(_))
}