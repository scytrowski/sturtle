package com.github.scytrowski.sturtle.es

final case class EventSourcingDescription[S, C, E, F[_]](initialState: S,
                                                         commandHandler: CommandHandler[C, E, F],
                                                         eventHandler: EventHandler[S, E, F])
