package com.github.scytrowski.sturtle.remoting

import java.net.InetSocketAddress

import cats.effect.syntax.concurrent._
import cats.effect.{Concurrent, ContextShift, Resource}
import com.github.scytrowski.sturtle.core.{TurtleEventSourcing, TurtleEventSourcingDescription, TurtleExtension, TurtleManager}
import com.github.scytrowski.sturtle.es.EventSourcing
import com.github.scytrowski.sturtle.remoting.server.{TurtleServer, TurtleServerHandler}
import com.github.scytrowski.sturtle.remoting.util.IdGenerator

final class RemotingExtension[F[_]: Concurrent: ContextShift](address: InetSocketAddress,
                                                              maxConcurrency: Int) extends TurtleExtension[F] {
  override def resource(manager: TurtleManager[F]): Resource[F, Unit] =
    TurtleServer
      .resource(address, IdGenerator.uuid[F])
      .flatMap(new TurtleServerHandler(manager.ref, maxConcurrency).handle(_).background)
      .map(_ => ())

  override def eventSourcing(description: TurtleEventSourcingDescription[F]): Resource[F, TurtleEventSourcing[F]] =
    Resource.pure(EventSourcing.basic(description))
}
