package com.github.scytrowski.sturtle.remoting.server

import cats.effect.IO
import cats.effect.concurrent.Ref
import com.github.scytrowski.sturtle.core.{TurtleCommand, TurtleQuery}
import com.github.scytrowski.sturtle.remoting.fixture.EffectSpecLike
import com.github.scytrowski.sturtle.remoting.mock.TestTurtleServerSideClient
import com.github.scytrowski.sturtle.remoting.protocol.{TurtleServerCommand, TurtleClientCommand}
import fs2.concurrent.Queue

class TurtleServerJobSchedulerTest extends EffectSpecLike {
  "TurtleServerJobScheduler" when {

    "schedule" should {

      "schedule job for SelectTurtle" in {
        val turtleId = "test-turtle"
        val (jobs, out) = scheduleJobs("select" -> TurtleServerCommand.SelectTurtle(turtleId)).runTimed()

        jobs.loneElement mustBe TurtleServerJob.SelectTurtle("select", turtleId)
        out.loneElement mustBe TurtleClientCommand.JobScheduled("select")
      }

      "schedule job for ReleaseTurtle" in {
        val (jobs, out) = scheduleJobs("release" -> TurtleServerCommand.ReleaseTurtle).runTimed()

        jobs.loneElement mustBe TurtleServerJob.ReleaseTurtle("release")
        out.loneElement mustBe TurtleClientCommand.JobScheduled("release")
      }

      "schedule job for RunCommand" in {
        val command = TurtleCommand.PenDown
        val (jobs, out) = scheduleJobs("run" -> TurtleServerCommand.RunCommand(command)).runTimed()

        jobs.loneElement mustBe TurtleServerJob.RunCommand("run", command)
        out.loneElement mustBe TurtleClientCommand.JobScheduled("run")
      }

      "schedule job for ExecuteQuery" in {
        val query = TurtleQuery.GetPenState
        val (jobs, out) = scheduleJobs("execute" -> TurtleServerCommand.ExecuteQuery(query)).runTimed()

        jobs.loneElement mustBe TurtleServerJob.ExecuteQuery("execute", query)
        out.loneElement mustBe TurtleClientCommand.JobScheduled("execute")
      }

    }

  }

  private def scheduleJobs(commandsWithIds: (String, TurtleServerCommand)*): IO[(List[TurtleServerJob], List[TurtleClientCommand])] = {
    for {
      jobIds   <- Ref.of[IO, List[String]](commandsWithIds.map(_._1).toList)
      jobId    = jobIds.modify(i => i.headOption.fold(List.empty[String] -> "finish")(i.tail -> _))
      jobQueue <- Queue.unbounded[IO, TurtleServerJob]
      clientData <- Ref.of[IO, TestTurtleServerSideClient.Data](TestTurtleServerSideClient.Data(commandsWithIds.map(_._2).toList))
      client   = new TestTurtleServerSideClient(clientData)
      scheduler = new TurtleServerJobScheduler(client, jobQueue, jobId)
      _ <- scheduler.schedule
      jobs <- jobQueue.dequeue.takeWhile({
        case _: TurtleServerJob.Finish => false
        case _ => true
      }).compile.toList
      cd   <- clientData.get
    } yield jobs -> cd.out

  }
}
