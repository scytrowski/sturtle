package com.github.scytrowski.sturtle.remoting

import cats.effect.{Blocker, Concurrent, ContextShift, Resource}
import com.github.scytrowski.sturtle.core.{TurtleController, TurtleRef}
import com.github.scytrowski.sturtle.remoting.client.{RawTurtleClientHandler, TurtleClientHandler}
import com.github.scytrowski.sturtle.remoting.net._
import fs2.io.tcp.SocketGroup

private[remoting] final class RemoteTurtleRef[F[_]: Concurrent: ContextShift](val id: String,
                                                                              connectionInfo: TurtleConnectionInfo) extends TurtleRef[F] {
  override def controller: Resource[F, TurtleController[F]] =
    Blocker[F]
      .flatMap(SocketGroup(_))
      .flatMap(_.client(connectionInfo.address))
      .evalMap(clientSideClient[F](clientId, _))
      .flatMap(RawTurtleClientHandler.resource[F])
      .map(new TurtleClientHandler(_))
      .flatMap(_.use(id))
      .map(new RemoteTurtleController(_))

  private val clientId: String =
    s"${connectionInfo.address}/$id"
}
