package com.github.scytrowski.sturtle.logging

import cats.Monad
import cats.effect.{Resource, Sync}
import com.github.scytrowski.sturtle.core.{TurtleEventSourcing, TurtleEventSourcingDescription, TurtleExtension, TurtleManager}
import com.github.scytrowski.sturtle.es.EventSourcing
import io.chrisdavenport.log4cats.Logger

final class LoggingExtension[F[_]: Sync](logger: TurtleLogger[F]) extends TurtleExtension[F] {
  override def resource(manager: TurtleManager[F]): Resource[F, Unit] = Resource.pure(())

  override def eventSourcing(description: TurtleEventSourcingDescription[F]): Resource[F, TurtleEventSourcing[F]] =
    Resource.pure {
      EventSourcing(description)(logger.logCommand)(logger.logEvents)(logger.logQuery)
    }
}

object LoggingExtension {
  def log4Cats[F[_]: Sync](logger: Logger[F], config: LoggingConfig): LoggingExtension[F] = {
    val turtleLogger = new Log4CatsTurtleLogger[F](logger, config)
    new LoggingExtension[F](turtleLogger)
  }
}
