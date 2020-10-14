package com.github.scytrowski.sturtle.remoting.client

import cats.effect.IO
import cats.effect.concurrent.Ref
import com.github.scytrowski.sturtle.core.TurtleQueryAnswer.AngleAnswer
import com.github.scytrowski.sturtle.core.geometry.Angle
import com.github.scytrowski.sturtle.remoting.fixture.EffectSpecLike
import com.github.scytrowski.sturtle.remoting.mock.{TestTurtleClientJobRegistry, TestTurtleClientSideClient}
import com.github.scytrowski.sturtle.remoting.protocol.TurtleClientCommand

class TurtleClientJobExecutorTest extends EffectSpecLike {
  "TurtleClientJobExecutor" when {

    "execute" should {

      "handle JobScheduled properly" in {
        val jobId = "test-job"
        val registryData = executeJobs(TurtleClientCommand.JobScheduled(jobId)).runTimed()

        registryData.promote.loneElement mustBe jobId
      }

      "handle TurtleSelected properly" in {
        val jobId = "test-job"
        val registryData = executeJobs(TurtleClientCommand.TurtleSelected(jobId)).runTimed()

        registryData.complete.loneElement mustBe jobId -> TurtleClientOperationResult.TurtleSelected
      }

      "handle TurtleReleased properly" in {
        val jobId = "test-job"
        val registryData = executeJobs(TurtleClientCommand.TurtleReleased(jobId)).runTimed()

        registryData.complete.loneElement mustBe jobId -> TurtleClientOperationResult.TurtleReleased
      }

      "handle CommandRan properly" in {
        val jobId = "test-job"
        val registryData = executeJobs(TurtleClientCommand.CommandRan(jobId)).runTimed()

        registryData.complete.loneElement mustBe jobId -> TurtleClientOperationResult.CommandRan
      }

      "handle QueryExecuted properly" in {
        val jobId = "test-job"
        val answer = AngleAnswer(Angle.radians(1.23))
        val registryData = executeJobs(TurtleClientCommand.QueryExecuted(jobId,answer)).runTimed()

        registryData.complete.loneElement mustBe jobId -> TurtleClientOperationResult.QueryExecuted(answer)
      }

      "handle NoTurtleSelected properly" in {
        val jobId = "test-job"
        val registryData = executeJobs(TurtleClientCommand.NoTurtleSelected(jobId)).runTimed()

        registryData.complete.loneElement mustBe jobId -> TurtleClientOperationResult.NoTurtleSelected
      }

    }

  }

  private def executeJobs(commands: TurtleClientCommand*): IO[TestTurtleClientJobRegistry.Data] =
    for {
      clientData <- Ref.of[IO, TestTurtleClientSideClient.Data](TestTurtleClientSideClient.Data(in = commands.toList))
      registryData <- Ref.of[IO, TestTurtleClientJobRegistry.Data](TestTurtleClientJobRegistry.Data())
      client = new TestTurtleClientSideClient(clientData)
      registry = new TestTurtleClientJobRegistry(registryData)
      executor = new TurtleClientJobExecutor(client, registry)
      _ <- executor.execute
      rd <- registryData.get
    } yield rd
}
