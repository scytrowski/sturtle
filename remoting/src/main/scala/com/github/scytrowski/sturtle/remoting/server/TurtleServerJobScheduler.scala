package com.github.scytrowski.sturtle.remoting.server

import cats.effect.Sync
import cats.syntax.functor._
import com.github.scytrowski.sturtle.remoting.net.TurtleServerSideClient
import com.github.scytrowski.sturtle.remoting.protocol.{TurtleServerCommand, TurtleClientCommand}
import fs2.Stream
import fs2.concurrent.Enqueue

private[remoting] final class TurtleServerJobScheduler[F[_]: Sync](client: TurtleServerSideClient[F],
                                                                   jobEnqueue: Enqueue[F, TurtleServerJob],
                                                                   jobId: F[String]) {
  def schedule: F[Unit] =
    client
      .receives
      .evalMap(handleCommand)
      .evalTap(jobScheduled)
      .append(Stream.eval(createJob(TurtleServerJob.Finish)))
      .through(jobEnqueue.enqueue)
      .compile
      .drain

  private def handleCommand(command: TurtleServerCommand): F[TurtleServerJob] =
    command match {
      case TurtleServerCommand.SelectTurtle(turtleId) =>
        createJob(TurtleServerJob.SelectTurtle(_, turtleId))
      case TurtleServerCommand.ReleaseTurtle =>
        createJob(TurtleServerJob.ReleaseTurtle)
      case TurtleServerCommand.RunCommand(command) =>
        createJob(TurtleServerJob.RunCommand(_, command))
      case TurtleServerCommand.ExecuteQuery(query) =>
        createJob(TurtleServerJob.ExecuteQuery(_, query))
    }

  private def createJob(job: String => TurtleServerJob): F[TurtleServerJob] = jobId.map(job)

  private def jobScheduled(job: TurtleServerJob): F[Unit] =
    client.send(TurtleClientCommand.JobScheduled(job.id))
}
