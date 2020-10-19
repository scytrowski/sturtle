package com.github.scytrowski.sturtle.es

import cats.effect.Sync
import cats.syntax.flatMap._
import cats.syntax.apply._
import cats.syntax.applicative._
import com.github.scytrowski.sturtle.es.EventDescription.{Event, Snapshot}
import io.chrisdavenport.log4cats.Logger
import io.chrisdavenport.log4cats.slf4j.Slf4jLogger

private[es] final class RecoveryHandler[F[_]: Sync, S, E](initialState: S,
                                                          eventHandler: EventHandler[F, S, E]) {
  def recover(data: List[EventDescription[S, E]]): F[S] = {
    val simplified = simplify(data)
    simplified
      .foldLeft(initialState.pure[F]) { case (f, d) => f.flatMap(doRecovery(_, d)) }
      .flatTap(s => logger.debug(s"Recovery completed with state $s"))
  }

  private def simplify(data: List[EventDescription[S, E]]): List[EventDescription[S, E]] = {
    val sorted = data.distinctBy(_.id).sortBy(_.timestamp)
    val lastSnapshotIndex = sorted.lastIndexWhere(isSnapshot, sorted.length)
    if (lastSnapshotIndex >= 0)
      sorted.drop(lastSnapshotIndex)
    else
      sorted
  }

  private def doRecovery(state: S, data: EventDescription[S, E]): F[S] =
    data match {
      case Event(id, timestamp, event) =>
        logger.debug(s"Recovering with event $id from $timestamp") *>
          eventHandler.handle(state, event)
      case Snapshot(id, timestamp, state) =>
        logger.debug(s"Recovering with snapshot $id from $timestamp") *>
          state.pure[F]
    }

  private def isSnapshot(data: EventDescription[S, E]): Boolean =
    data match {
      case EventDescription.Snapshot(_, _, _) => true
      case _ => false
    }

  private val logger: Logger[F] = Slf4jLogger.getLogger[F]
}
