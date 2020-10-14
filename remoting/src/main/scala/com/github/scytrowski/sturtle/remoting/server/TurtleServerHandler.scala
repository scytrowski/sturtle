package com.github.scytrowski.sturtle.remoting.server

import cats.effect.Concurrent
import cats.effect.syntax.concurrent._
import cats.syntax.flatMap._
import cats.syntax.functor._
import com.github.scytrowski.sturtle.core.TurtleRefProvider
import com.github.scytrowski.sturtle.remoting.net.TurtleServerSideClient
import com.github.scytrowski.sturtle.remoting.util.IdGenerator
import fs2.concurrent.Queue

private[remoting] object TurtleServerHandler {
  def handle[F[_]: Concurrent](server: TurtleServer[F],
                               refProvider: TurtleRefProvider[F],
                               maxConcurrency: Int): F[Unit] =
    server
      .clients
      .parEvalMap(maxConcurrency)(_.use(handleClient(refProvider)))
      .compile
      .drain

  private def handleClient[F[_]: Concurrent](refProvider: TurtleRefProvider[F])(client: TurtleServerSideClient[F]): F[Unit] =
    for {
      jobQueue       <- Queue.unbounded[F, TurtleServerJob]
      schedulerFiber <- new TurtleServerJobScheduler(client, jobQueue, IdGenerator.uuid[F]).schedule.start
      executorFiber  <- new TurtleServerJobExecutor(refProvider, client, jobQueue).execute.start
      _              <- schedulerFiber.join
      _              <- executorFiber.join
    } yield ()
}
