package com.github.scytrowski.sturtle.remoting.client

import cats.effect.{Resource, Sync}
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.apply._
import com.github.scytrowski.sturtle.core.{TurtleCommand, TurtleQuery, TurtleQueryAnswer}
import com.github.scytrowski.sturtle.remoting.protocol.TurtleServerCommand
import io.chrisdavenport.log4cats.StructuredLogger
import io.chrisdavenport.log4cats.slf4j.Slf4jLogger

private[remoting] final class TurtleClientHandler[F[_]: Sync](override protected val rawHandler: RawTurtleClientHandler[F]) extends TurtleClientHandlerBase[F] {
  def use(turtleId: String): Resource[F, AcquiredTurtleClientHandler[F]] =
    Resource
      .make(select(turtleId))(_ => release)
      .map(_ => new AcquiredTurtleClientHandler(rawHandler))

  private def select(turtleId: String): F[Unit] =
    executeCommand(TurtleServerCommand.SelectTurtle(turtleId)) {
      case TurtleClientOperationResult.TurtleSelected =>
    }

  private def release: F[Unit] =
    executeCommand(TurtleServerCommand.ReleaseTurtle) {
      case TurtleClientOperationResult.TurtleReleased =>
    }
}

private[remoting] final class AcquiredTurtleClientHandler[F[_]: Sync](override protected val rawHandler: RawTurtleClientHandler[F]) extends TurtleClientHandlerBase[F] {
  def run(command: TurtleCommand): F[Unit] =
    executeCommand(TurtleServerCommand.RunCommand(command)) {
      case TurtleClientOperationResult.CommandRan =>
    }

  def execute(query: TurtleQuery): F[TurtleQueryAnswer] =
    executeCommand(TurtleServerCommand.ExecuteQuery(query)) {
      case TurtleClientOperationResult.QueryExecuted(answer) => answer
    }
}

private[remoting] abstract class TurtleClientHandlerBase[F[_]: Sync] {
  protected def rawHandler: RawTurtleClientHandler[F]

  final protected def executeCommand[A](command: TurtleServerCommand)(pf: PartialFunction[TurtleClientOperationResult, A]): F[A] =
    rawHandler
      .executeCommand(command)
      .flatMap { result =>
        pf.lift(result) match {
          case Some(a) =>
            logger.debug(mdc)(s"Command $command has been executed with result $a").as(a)
          case None =>
            logger.error(mdc)(s"Invalid result $result for command $command") *>
              Sync[F].raiseError(new IllegalStateException(s"Invalid operation $command result: $result"))
        }
      }

  final protected val mdc: Map[String, String] = Map("clientId" -> rawHandler.clientId)

  final protected val logger: StructuredLogger[F] = Slf4jLogger.getLogger[F]
}