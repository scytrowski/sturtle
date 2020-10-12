package com.github.scytrowski.sturtle.remoting.server

import cats.effect.Concurrent
import cats.effect.syntax.concurrent._
import cats.syntax.option._
import cats.syntax.apply._
import cats.syntax.flatMap._
import cats.syntax.functor._
import com.github.scytrowski.sturtle.core.{TurtleController, TurtleQueryAnswer, TurtleRef}
import com.github.scytrowski.sturtle.remoting.net.TurtleServerSideClient
import com.github.scytrowski.sturtle.remoting.protocol.TurtleClientCommand
import fs2.concurrent.Dequeue1
import io.chrisdavenport.log4cats.StructuredLogger
import io.chrisdavenport.log4cats.slf4j.Slf4jLogger

private[remoting] final class TurtleServerJobExecutor[F[_]: Concurrent](turtleRef: String => F[TurtleRef[F]],
                                                      client: TurtleServerSideClient[F],
                                                      jobDequeue: Dequeue1[F, TurtleServerJob]) {
  def execute: F[Unit] =
    jobDequeue.dequeue1
      .flatTap(job => logger.debug(mdc(job.id))(s"Starting execution of server job: $job"))
      .flatMap {
        case TurtleServerJob.SelectTurtle(id, turtleId) =>
          turtleRef(turtleId)
            .flatMap(_.controller.use(selectTurtle(id, turtleId)))
            .start
            .flatMap(_.join)
            .flatMap {
              case true => execute
              case false => Concurrent[F].unit
            }
        case TurtleServerJob.Finish(id) =>
          logger.debug(mdc(id))(s"Finished execution of jobs for client: ${client.id}")
        case job: TurtleServerJob =>
          logger.debug(mdc(job.id))(s"No turtle selected for job: $job") *>
            noTurtleSelected(job.id) *>
            execute
      }

  private def executeWithTurtle(acquiredTurtleId: String, controller: TurtleController[F]): F[Boolean] =
    jobDequeue.dequeue1.flatMap {
      case TurtleServerJob.SelectTurtle(id, turtleId) =>
        if (turtleId == acquiredTurtleId)
          selectTurtle(id, turtleId)(controller)
        else
          turtleRef(turtleId)
            .flatMap(_.controller.use(selectTurtle(id, turtleId)))
            .start
            .flatMap(_.join)
      case TurtleServerJob.ReleaseTurtle(id) =>
        logger.debug(mdc(id, acquiredTurtleId.some))(s"Released turtle $acquiredTurtleId") *>
          turtleReleased(id).as(true)
      case TurtleServerJob.RunCommand(id, command) =>
        controller.run(command) *>
          logger.debug(mdc(id, acquiredTurtleId.some))(s"Command $command has been ran for turtle $acquiredTurtleId") *>
          commandRan(id) *>
          executeWithTurtle(acquiredTurtleId, controller)
      case TurtleServerJob.ExecuteQuery(id, query) =>
        controller
          .execute(query)
          .flatTap(answer => logger.debug(mdc(id, acquiredTurtleId.some))(s"Query $query has been executed with result $answer for turtle $acquiredTurtleId"))
          .flatMap(queryExecuted(id, _)) *>
          executeWithTurtle(acquiredTurtleId, controller)
      case TurtleServerJob.Finish(id) =>
        logger.debug(mdc(id, acquiredTurtleId.some))(s"Finished execution of jobs for client: ${client.id}").as(false)
    }

  private def selectTurtle(jobId: String, turtleId: String)(controller: TurtleController[F]): F[Boolean] =
    logger.debug(mdc(jobId, turtleId.some))(s"Selected turtle $turtleId for job execution") *>
      turtleSelected(jobId) *>
      executeWithTurtle(turtleId, controller)

  private def noTurtleSelected(jobId: String): F[Unit] =
    sendCommand(TurtleClientCommand.NoTurtleSelected(jobId))

  private def turtleSelected(jobId: String): F[Unit] =
    sendCommand(TurtleClientCommand.TurtleSelected(jobId))

  private def turtleReleased(jobId: String): F[Unit] =
    sendCommand(TurtleClientCommand.TurtleReleased(jobId))

  private def commandRan(jobId: String): F[Unit] =
    sendCommand(TurtleClientCommand.CommandRan(jobId))

  private def queryExecuted(jobId: String, answer: TurtleQueryAnswer): F[Unit] =
    sendCommand(TurtleClientCommand.QueryExecuted(jobId, answer))

  private def sendCommand(command: TurtleClientCommand): F[Unit] =
    client.send(command)

  private def mdc(jobId: String, turtleId: Option[String] = None): Map[String, String] =
    Map(
      "clientId" -> client.id,
      "jobId" -> jobId
    ) ++ turtleId.map("turtleId" -> _)

  private val logger: StructuredLogger[F] = Slf4jLogger.getLogger[F]
}
