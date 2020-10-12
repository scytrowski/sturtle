package com.github.scytrowski.sturtle.remoting.server

import cats.effect.Concurrent
import cats.effect.syntax.concurrent._
import cats.syntax.flatMap._
import cats.syntax.functor._
import com.github.scytrowski.sturtle.core.TurtleRef
import com.github.scytrowski.sturtle.remoting.net.TurtleServerSideClient
import com.github.scytrowski.sturtle.remoting.util.IdGenerator
import fs2.concurrent.Queue

private[remoting] final class TurtleServerHandler[F[_]: Concurrent](turtleRef: String => F[TurtleRef[F]],
                                                  maxConcurrency: Int) {
  def handle(server: TurtleServer[F]): F[Unit] =
    server
      .clients
      .parEvalMap(maxConcurrency)(_.use(handleClient))
      .compile
      .drain

  private def handleClient(client: TurtleServerSideClient[F]): F[Unit] =
    for {
      jobQueue       <- Queue.unbounded[F, TurtleServerJob]
      schedulerFiber <- new TurtleServerJobScheduler(client, jobQueue, IdGenerator.uuid[F]).schedule.start
      executorFiber  <- new TurtleServerJobExecutor(turtleRef, client, jobQueue).execute.start
      _              <- schedulerFiber.join
      _              <- executorFiber.join
    } yield ()
}
