package com.github.scytrowski.sturtle.remoting.client

import cats.effect.syntax.concurrent._
import cats.effect.{Concurrent, Resource}
import cats.syntax.flatMap._
import cats.syntax.functor._
import com.github.scytrowski.sturtle.remoting.net.{TurtleClient, TurtleClientSideClient}
import com.github.scytrowski.sturtle.remoting.protocol.{TurtleClientCommand, TurtleServerCommand}
import io.chrisdavenport.log4cats.StructuredLogger
import io.chrisdavenport.log4cats.slf4j.Slf4jLogger

private[remoting] final class RawTurtleClientHandler[F[_]: Concurrent](client: TurtleClient[F, TurtleClientCommand, TurtleServerCommand],
                                                                     jobRegistry: TurtleClientJobRegistry[F]) {
  def clientId: String = client.id

  def executeCommand(command: TurtleServerCommand): F[TurtleClientOperationResult] =
    for {
      _          <- logger.debug(mdc)(s"Registering command $command for execution")
      completion <- jobRegistry.register
      _          <- client.send(command)
      result     <- completion
    } yield result

  private val mdc: Map[String, String] = Map("clientId" -> client.id)

  private val logger: StructuredLogger[F] = Slf4jLogger.getLogger[F]
}

private[remoting] object RawTurtleClientHandler {
  def resource[F[_]: Concurrent](client: TurtleClientSideClient[F]): Resource[F, RawTurtleClientHandler[F]] =
    for {
      jobRegistry  <- Resource.liftF(SynchronizedTurtleClientJobRegistry.make[F])
      _            <- new TurtleClientJobExecutor(client, jobRegistry).execute.background
    } yield new RawTurtleClientHandler(client, jobRegistry)
}
