package com.github.scytrowski.sturtle.remoting.client

import cats.effect.Concurrent
import cats.effect.concurrent.{Deferred, Ref}
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.apply._
import fs2.concurrent.Queue
import io.chrisdavenport.log4cats.StructuredLogger
import io.chrisdavenport.log4cats.slf4j.Slf4jLogger

private[remoting] trait TurtleClientJobRegistry[F[_]] {
  def register: F[F[TurtleClientOperationResult]]

  def promote(id: String): F[Unit]

  def complete(id: String, result: TurtleClientOperationResult): F[Unit]
}

private[remoting] final class SynchronizedTurtleClientJobRegistry[F[_]: Concurrent] private[client](stagingQueue: Queue[F, Deferred[F, TurtleClientOperationResult]],
                                                                                                    jobs: Ref[F, Map[String, Deferred[F, TurtleClientOperationResult]]]) extends TurtleClientJobRegistry[F] {
  def register: F[F[TurtleClientOperationResult]] =
    for {
      df <- Deferred[F, TurtleClientOperationResult]
      _  <- stagingQueue.enqueue1(df)
      _  <- logger.debug("New job has been registered")
    } yield df.get

  def promote(id: String): F[Unit] =
    stagingQueue.tryDequeue1.flatMap {
      case Some(df) =>
        jobs.update(_.updated(id, df)) *>
          logger.debug(mdc(id))(s"Job $id has been promoted")
      case None     =>
        logger.warn(mdc(id))(s"Cannot promote job $id because there is no registered staged job")
    }

  def complete(id: String, result: TurtleClientOperationResult): F[Unit] =
    jobs.modify { jobs =>
      (jobs - id) -> jobs.get(id)
    }.flatMap {
      case Some(df) =>
        df.complete(result) *>
          logger.debug(mdc(id))(s"Job $id has been completed with result $result")
      case None =>
        logger.warn(mdc(id))(s"Cannot complete job $id because there is not such promoted job")
    }

  private def mdc(jobId: String): Map[String, String] = Map("jobId" -> jobId)

  private val logger: StructuredLogger[F] = Slf4jLogger.getLogger[F]
}

private[remoting] object SynchronizedTurtleClientJobRegistry {
  def make[F[_]: Concurrent]: F[SynchronizedTurtleClientJobRegistry[F]] =
    for {
      stagingQueue <- Queue.unbounded[F, Deferred[F, TurtleClientOperationResult]]
      jobs         <- Ref.of(Map.empty[String, Deferred[F, TurtleClientOperationResult]])
    } yield new SynchronizedTurtleClientJobRegistry(stagingQueue, jobs)
}
