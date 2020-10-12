package com.github.scytrowski.sturtle.remoting.client

import cats.syntax.apply._
import cats.effect.Concurrent
import com.github.scytrowski.sturtle.remoting.net.TurtleClientSideClient
import com.github.scytrowski.sturtle.remoting.protocol.TurtleClientCommand
import io.chrisdavenport.log4cats.StructuredLogger
import io.chrisdavenport.log4cats.slf4j.Slf4jLogger

private[remoting] final class TurtleClientJobExecutor[F[_]: Concurrent](client: TurtleClientSideClient[F],
                                                                        jobRegistry: TurtleClientJobRegistry[F]) {
  def execute: F[Unit] =
    client
      .receives
      .evalMap(handleCommand)
      .compile
      .drain

  private def handleCommand(command: TurtleClientCommand): F[Unit] =
    command match {
      case TurtleClientCommand.JobScheduled(id) =>
        promoteJob(id)
      case TurtleClientCommand.TurtleSelected(id) =>
        completeJob(id, TurtleClientOperationResult.TurtleSelected)
      case TurtleClientCommand.TurtleReleased(id) =>
        completeJob(id, TurtleClientOperationResult.TurtleReleased)
      case TurtleClientCommand.CommandRan(id) =>
        completeJob(id, TurtleClientOperationResult.CommandRan)
      case TurtleClientCommand.QueryExecuted(id, answer) =>
        completeJob(id, TurtleClientOperationResult.QueryExecuted(answer))
      case TurtleClientCommand.NoTurtleSelected(id) =>
        completeJob(id, TurtleClientOperationResult.NoTurtleSelected)
    }

  private def promoteJob(id: String): F[Unit] =
    jobRegistry.promote(id) *>
      logger.debug(mdc(id))(s"Job $id has been promoted")

  private def completeJob(id: String, result: TurtleClientOperationResult): F[Unit] =
    jobRegistry.complete(id, result) *>
      logger.debug(mdc(id))(s"Job $id has been completed with result $result")

  private def mdc(jobId: String): Map[String, String] =
    Map(
      "clientId" -> client.id,
      "jobId" -> jobId
    )

  private val logger: StructuredLogger[F] = Slf4jLogger.getLogger[F]
}
