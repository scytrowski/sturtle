package com.github.scytrowski.sturtle.es

final case class EventSourcingDescription[F[_], S, C, E, Q <: Query[S]](initialState: S,
                                                                        commandHandler: CommandHandler[F, S, C, E],
                                                                        eventHandler: EventHandler[F, S, E],
                                                                        queryHandler: QueryHandler[F, S, Q])
