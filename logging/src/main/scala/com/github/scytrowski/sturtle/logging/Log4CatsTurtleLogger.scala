package com.github.scytrowski.sturtle.logging
import cats.Applicative
import cats.syntax.traverse._
import cats.syntax.functor._
import cats.instances.list._
import com.github.scytrowski.sturtle.core.{TurtleCommand, TurtleEvent, TurtleQuery}
import io.chrisdavenport.log4cats.Logger

final class Log4CatsTurtleLogger[F[_]: Applicative](logger: Logger[F], config: LoggingConfig) extends TurtleLogger[F] {
  override def logCommand(command: TurtleCommand): F[Unit] =
    log(config.commandLogLevel, s"Handling turtle command: $command")

  override def logEvents(events: List[TurtleEvent]): F[Unit] =
    events
      .map(logEvent)
      .sequence
      .as(())

  override def logQuery(query: TurtleQuery): F[Unit] =
    log(config.queryLogLevel, s"Handling turtle query: $query")

  private def logEvent(event: TurtleEvent): F[Unit] =
    log(config.eventLogLevel, s"Handling turtle event: $event")

  private def log(logLevel: LogLevel, message: String): F[Unit] =
    logLevel match {
      case LogLevel.Error => logger.error(message)
      case LogLevel.Warn => logger.warn(message)
      case LogLevel.Info => logger.info(message)
      case LogLevel.Debug => logger.debug(message)
      case LogLevel.Trace => logger.trace(message)
      case LogLevel.Disabled => Applicative[F].unit
    }
}
