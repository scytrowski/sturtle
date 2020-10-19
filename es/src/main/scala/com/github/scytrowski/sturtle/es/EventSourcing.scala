package com.github.scytrowski.sturtle.es

import cats.effect.concurrent.MVar
import cats.effect.{Concurrent, Resource}
import cats.syntax.flatMap._
import io.chrisdavenport.log4cats.Logger
import io.chrisdavenport.log4cats.slf4j.Slf4jLogger

final class EventSourcing[F[_]: Concurrent, I, S, C, E, Q <: Query[S]](description: EventSourcingDescription[F, S, C, E, Q],
                                                                       eventStore: EventStore[F, I, S, E],
                                                                       sinks: EventSourcingSinks[F, S, C, E, Q],
                                                                       lockManager: EntityLockManager[F, I]) { self =>
  def entity(id: I): Resource[F, EventSourcedEntity[F, S, C, E, Q]] =
    for {
      _        <- retrieveLock(id)
      state    <- recover(id)
      session  <- eventStore.session(id)
      stateVar <- Resource.liftF(MVar.of(state))
    } yield new EventSourcedEntity(description, sinks, session, stateVar)

  private def retrieveLock(id: I): Resource[F, Unit] =
    Resource
      .liftF(logger.debug(s"Acquiring lock for entity $id..."))
      .flatMap(_ => lockManager.retrieve(id))
      .evalTap(_ => logger.debug(s"Lock for entity $id has been acquired"))

  private def recover(id: I): Resource[F, S] =
    Resource.liftF {
      eventStore
        .retrieve(id)
        .flatMap(recoveryHandler.recover)
    }

  private val recoveryHandler: RecoveryHandler[F, S, E] = new RecoveryHandler(description.initialState, description.eventHandler)

  private val logger: Logger[F] = Slf4jLogger.getLogger[F]
}
