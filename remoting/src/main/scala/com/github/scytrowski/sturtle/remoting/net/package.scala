package com.github.scytrowski.sturtle.remoting

import java.io.IOException
import java.net.InetSocketAddress

import cats.effect.Sync
import cats.syntax.flatMap._
import cats.syntax.functor._
import com.github.scytrowski.sturtle.remoting.protocol.{TurtleClientCommand, TurtleServerCommand}
import fs2.io.tcp.Socket

package object net {
  type TurtleServerSideClient[F[_]] = TurtleClient[F, TurtleServerCommand, TurtleClientCommand]
  type TurtleClientSideClient[F[_]] = TurtleClient[F, TurtleClientCommand, TurtleServerCommand]

  def serverSideClient[F[_]: Sync](id: String, socket: Socket[F]): F[TurtleServerSideClient[F]] =
    retrieveAddress(socket)
      .map(new TcpTurtleClient(id, socket, _, TurtleServerCommand.codec, TurtleClientCommand.codec))

  def clientSideClient[F[_]: Sync](id: String, socket: Socket[F]): F[TurtleClientSideClient[F]] =
    retrieveAddress(socket)
      .map(new TcpTurtleClient(id, socket, _, TurtleClientCommand.codec, TurtleServerCommand.codec))

  private def retrieveAddress[F[_]: Sync](socket: Socket[F]): F[InetSocketAddress] =
    socket
      .remoteAddress
      .flatMap {
        case addr: InetSocketAddress => Sync[F].pure(addr)
        case invalid => Sync[F].raiseError(new IOException(s"Invalid socket address: $invalid"))
      }
}
