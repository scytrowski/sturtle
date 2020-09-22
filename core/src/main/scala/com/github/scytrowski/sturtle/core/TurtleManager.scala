package com.github.scytrowski.sturtle.core

import cats.data.NonEmptyList
import cats.effect.concurrent.{MVar, MVar2, Semaphore}
import cats.effect.{Concurrent, Resource}
import cats.syntax.functor._
import cats.syntax.traverse._
import cats.{Applicative, Monad}
import com.github.scytrowski.sturtle.es.{EventSourcing, EventSourcingDescription}

final class TurtleManager[F[_]: Concurrent] private(eventSourcing: TurtleEventSourcing[F],
                                                    turtleLocks: MVar2[F, Map[String, Semaphore[F]]]) {
  def ref(id: String): F[TurtleRef[F]] =
    retrieveLock(id).map(new LocalTurtleRef(id, eventSourcing, _))

  private def retrieveLock(id: String): F[Semaphore[F]] =
    turtleLocks.modify { locks =>
      locks.get(id) match {
        case Some(lock) => Applicative[F].pure(locks -> lock)
        case None => createLock.map(lock => (locks + (id -> lock)) -> lock)
      }
    }

  private def createLock: F[Semaphore[F]] = Semaphore[F](1)
}

object TurtleManager {
  def resource[F[_]: Concurrent](extensions: List[TurtleExtension[F]]): Resource[F, TurtleManager[F]] =
    for {
      es <- buildEventSourcing(extensions)
      locks <- createTurtleLocks
    } yield new TurtleManager(es, locks)

  private def buildEventSourcing[F[_]: Monad](extensions: List[TurtleExtension[F]]): Resource[F, TurtleEventSourcing[F]] = {
    val desc = description[F]
    NonEmptyList
      .of(Resource.pure(EventSourcing.basic(desc)), extensions.map(_.eventSourcing(desc)):_*)
      .sequence
      .map(_.reduceLeft(_ andThen _))
  }

  private def createTurtleLocks[F[_]: Concurrent]: Resource[F, TurtleLocks[F]] =
    Resource.liftF(MVar.of[F, Map[String, Semaphore[F]]](Map.empty))

  private def description[F[_]: Applicative]: TurtleEventSourcingDescription[F] =
    EventSourcingDescription(Turtle.initial, TurtleCommand.handler, TurtleEvent.handler, TurtleQuery.handler)

  private type TurtleLocks[F[_]] = MVar2[F, Map[String, Semaphore[F]]]
}
