package com.github.scytrowski.sturtle.es

import cats.Monad
import cats.effect.Sync
import cats.syntax.flatMap._
import io.chrisdavenport.log4cats.Logger
import io.chrisdavenport.log4cats.slf4j.Slf4jLogger

abstract class EventSourcing[F[_]: Sync, S, C, E, Q <: Query[S]](description: EventSourcingDescription[F, S, C, E, Q]) { self =>
  def commandSink(command: C): F[Unit]
  def eventSink(events: List[E]): F[Unit]
  def querySink(query: Q): F[Unit]

  final def run(state: S, command: C): F[S] = {
    logger.debug(s"Handling command $command on state $state") >>
      commandSink(command) >>
      description.commandHandler
        .handle(state, command)
        .flatTap(events => logger.debug(s"Handling events ${events.mkString(", ")} on state $state"))
        .flatTap(eventSink)
        .flatMap(description.eventHandler.handleMany(state, _))
  }

  final def execute(state: S, query: Q): F[query.Answer] = {
    logger.debug(s"Executing query $query on state $state") >>
      querySink(query) >>
        description.queryHandler
          .handle(state, query)
          .flatTap(answer => logger.debug(s"Executed query $query with answer $answer on state $state"))
  }


  final def recover(data: List[RecoveryData[S, E]]): F[S] = recoveryHandler.recover(data)

  final def andThen(other: EventSourcing[F, S, C, E, Q]): EventSourcing[F, S, C, E, Q] =
    EventSourcing(description)(c => self.commandSink(c) >> other.commandSink(c))(e => self.eventSink(e) >> other.eventSink(e))(q => self.querySink(q) >> other.querySink(q))

  private val recoveryHandler: RecoveryHandler[F, S, E] = new RecoveryHandler(description.initialState, description.eventHandler)

  private val logger: Logger[F] = Slf4jLogger.getLogger[F]
}

object EventSourcing {
  def apply[F[_]: Sync, S, C, E, Q <: Query[S]](description: EventSourcingDescription[F, S, C, E, Q])
                                             (cf: C => F[Unit])
                                             (ef: List[E] => F[Unit])
                                             (qf: Q => F[Unit]): EventSourcing[F, S, C, E, Q] =
    new EventSourcing[F, S, C, E, Q](description) {
      override def commandSink(command: C): F[Unit] = cf(command)

      override def eventSink(events: List[E]): F[Unit] = ef(events)

      override def querySink(query: Q): F[Unit] = qf(query)
    }

  def basic[F[_]: Sync, S, C, E, Q <: Query[S]](description: EventSourcingDescription[F, S, C, E, Q]): EventSourcing[F, S, C, E, Q] =
    EventSourcing(description)(_ => Monad[F].unit)(_ => Monad[F].unit)(_ => Monad[F].unit)
}
