package com.github.scytrowski.sturtle.remoting

import java.net.InetSocketAddress

import cats.effect.{Concurrent, ContextShift, Resource}
import cats.effect.syntax.concurrent._
import com.github.scytrowski.sturtle.core.TurtleRefProvider
import com.github.scytrowski.sturtle.remoting.server.{TurtleServer, TurtleServerHandler}
import com.github.scytrowski.sturtle.remoting.util.IdGenerator

object TurtleRemoting {
  def start[F[_]: Concurrent: ContextShift](connectionInfo: TurtleConnectionInfo,
                                            refProvider: TurtleRefProvider[F],
                                            maxConcurrency: Int): Resource[F, InetSocketAddress] =
    for {
      server <- TurtleServer.resource[F](connectionInfo.address, IdGenerator.uuid)
      _      <- TurtleServerHandler.handle[F](server, refProvider, maxConcurrency).background
    } yield server.address
}
