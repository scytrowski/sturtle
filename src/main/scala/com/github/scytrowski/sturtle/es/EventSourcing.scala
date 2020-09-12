package com.github.scytrowski.sturtle.es

import cats.Monad
import cats.syntax.flatMap._

abstract class EventSourcing[S, C, E, F[_]: Monad](description: EventSourcingDescription[S, C, E, F]) {
  def eventSink(event: List[E]): F[Unit]

  final def run(state: S, command: C): F[S] =
    description.commandHandler
      .handle(command)
      .flatTap(eventSink)
      .flatMap(description.eventHandler.handleMany(state, _))

  final def recover(events: List[E]): F[S] =
    description.eventHandler
      .handleMany(description.initialState, events)
}
