package com.github.scytrowski.sturtle.remoting.server

import java.net.InetSocketAddress

import cats.effect.{Blocker, Concurrent, ContextShift, Resource, Sync}
import cats.syntax.flatMap._
import cats.syntax.functor._
import com.github.scytrowski.sturtle.remoting.net._
import fs2.Stream
import fs2.io.tcp.{Socket, SocketGroup}
import io.chrisdavenport.log4cats.StructuredLogger
import io.chrisdavenport.log4cats.slf4j.Slf4jLogger

private[remoting] trait TurtleServer[F[_]] {
  def address: InetSocketAddress

  def clients: Stream[F, Resource[F, TurtleServerSideClient[F]]]
}

private[remoting] object TurtleServer {
  def resource[F[_]: Concurrent: ContextShift](address: InetSocketAddress,
                                               clientId: F[String]): Resource[F, TurtleServer[F]] = {
    Blocker[F]
      .evalTap(_ => logger.debug(s"Starting turtle server on $address..."))
      .flatMap(SocketGroup(_))
      .flatMap(_.serverResource(address))
      .map { case (address, clients) =>
        val tcpClients = clients.map(createClient[F](clientId, logger, _))
        new TcpTurtleServer(address, tcpClients)
      }
      .evalTap(server => logger.info(s"Turtle server started on ${server.address}"))
  }

  private def createClient[F[_]: Sync](clientId: F[String],
                                       logger: StructuredLogger[F],
                                       resource: Resource[F, Socket[F]]): Resource[F, TurtleServerSideClient[F]] =
    resource
      .evalMap { socket =>
        for {
          id     <- clientId
          client <- serverSideClient(id, socket)
        } yield client
      }
      .evalTap(client => logger.info(s"Accepted new turtle client connection from ${client.address}"))

  private def logger[F[_]: Sync]: StructuredLogger[F] = Slf4jLogger.getLogger[F]
}

private[remoting] final class TcpTurtleServer[F[_]: Concurrent: ContextShift](val address: InetSocketAddress,
                                                                              val clients: Stream[F, Resource[F, TurtleServerSideClient[F]]]) extends TurtleServer[F]
