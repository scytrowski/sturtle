package com.github.scytrowski.sturtle.es

import cats.Monad
import cats.syntax.flatMap._

abstract class EventSourcing[S, C, E, F[_]: Monad](description: EventSourcingDescription[S, C, E, F]) { self =>
  def commandSink(command: C): F[Unit]
  def eventSink(events: List[E]): F[Unit]

  final def run(state: S, command: C): F[S] =
    commandSink(command) >>
      description.commandHandler
        .handle(command)
        .flatTap(eventSink)
        .flatMap(description.eventHandler.handleMany(state, _))

  final def recover(events: List[E]): F[S] =
    description.eventHandler
      .handleMany(description.initialState, events)

  final def andThen(other: EventSourcing[S, C, E, F]): EventSourcing[S, C, E, F] =
    EventSourcing(description)(c => self.commandSink(c) >> other.commandSink(c))(e => self.eventSink(e) >> other.eventSink(e))
}

object EventSourcing {
  def apply[S, C, E, F[_]: Monad](description: EventSourcingDescription[S, C, E, F])(cf: C => F[Unit])(ef: List[E] => F[Unit]): EventSourcing[S, C, E, F] =
    new EventSourcing[S, C, E, F](description) {
      override def commandSink(command: C): F[Unit] = cf(command)

      override def eventSink(events: List[E]): F[Unit] = ef(events)
    }

  def basic[S, C, E, F[_]: Monad](description: EventSourcingDescription[S, C, E, F]): EventSourcing[S, C, E, F] =
    EventSourcing(description)(_ => Monad[F].unit)(_ => Monad[F].unit)
}
