package com.github.scytrowski.sturtle.es

import cats.effect.Sync
import cats.effect.concurrent.MVar2
import cats.syntax.flatMap._
import io.chrisdavenport.log4cats.Logger
import io.chrisdavenport.log4cats.slf4j.Slf4jLogger

final class EventSourcedEntity[F[_]: Sync, S, C, E, Q <: Query[S]] private[es](description: EventSourcingDescription[F, S, C, E, Q],
                                                                               sinks: EventSourcingSinks[F, S, C, E, Q],
                                                                               session: EventStoreSession[F, E],
                                                                               stateVar: MVar2[F, S]) {
  def run(command: C): F[Unit] = {
    stateVar.modify_ { state =>
      logger.debug(s"Running command $command on state $state") >>
        sinks.commandSink(command) >>
          description
            .commandHandler
            .handle(state, command)
            .flatTap(events => logger.debug(s"Handling events ${events.mkString(", ")} on state $state"))
            .flatTap(session.persist)
            .flatTap(sinks.eventSink)
            .flatMap(description.eventHandler.handleMany(state, _))
    }
  }

  def execute(query: Q): F[query.Answer] =
    stateVar.use { state =>
      logger.debug(s"Executing query $query on state $state") >>
        sinks.querySink(query) >>
          description
            .queryHandler
            .handle(state, query)
            .flatTap(answer => logger.debug(s"Query $query has been executed with result: $answer"))
    }

  private val logger: Logger[F] = Slf4jLogger.getLogger[F]
}
