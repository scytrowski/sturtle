package com.github.scytrowski.sturtle.es

final case class EventSourcingDescription[S, C, E, Q <: Query[S], F[_]](initialState: S,
                                                                     commandHandler: CommandHandler[S, C, E, F],
                                                                     eventHandler: EventHandler[S, E, F],
                                                                     queryHandler: QueryHandler[S, Q, F])
