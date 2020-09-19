package com.github.scytrowski.sturtle.es

import cats.Monad
import cats.syntax.flatMap._

abstract class EventSourcing[S, C, E, Q <: Query[S], F[_]: Monad](description: EventSourcingDescription[S, C, E, Q, F]) { self =>
  def commandSink(command: C): F[Unit]
  def eventSink(events: List[E]): F[Unit]
  def querySink(query: Q): F[Unit]

  final def run(state: S, command: C): F[S] =
    commandSink(command) >>
      description.commandHandler
        .handle(command)
        .flatTap(eventSink)
        .flatMap(description.eventHandler.handleMany(state, _))

  final def execute(state: S, query: Q): F[query.Answer] =
    querySink(query) >>
      description.queryHandler
        .handle(state, query)


  final def recover(events: List[E]): F[S] =
    description.eventHandler
      .handleMany(description.initialState, events)

  final def andThen(other: EventSourcing[S, C, E, Q, F]): EventSourcing[S, C, E, Q, F] =
    EventSourcing(description)(c => self.commandSink(c) >> other.commandSink(c))(e => self.eventSink(e) >> other.eventSink(e))(q => self.querySink(q) >> other.querySink(q))
}

object EventSourcing {
  def apply[S, C, E, Q <: Query[S], F[_]: Monad](description: EventSourcingDescription[S, C, E, Q, F])
                                             (cf: C => F[Unit])
                                             (ef: List[E] => F[Unit])
                                             (qf: Q => F[Unit]): EventSourcing[S, C, E, Q, F] =
    new EventSourcing[S, C, E, Q, F](description) {
      override def commandSink(command: C): F[Unit] = cf(command)

      override def eventSink(events: List[E]): F[Unit] = ef(events)

      override def querySink(query: Q): F[Unit] = qf(query)
    }

  def basic[S, C, E, Q <: Query[S], F[_]: Monad](description: EventSourcingDescription[S, C, E, Q, F]): EventSourcing[S, C, E, Q, F] =
    EventSourcing(description)(_ => Monad[F].unit)(_ => Monad[F].unit)(_ => Monad[F].unit)
}
