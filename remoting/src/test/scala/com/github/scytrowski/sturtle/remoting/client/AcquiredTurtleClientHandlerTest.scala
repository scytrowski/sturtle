package com.github.scytrowski.sturtle.remoting.client

import cats.effect.IO
import cats.effect.concurrent.Ref
import com.github.scytrowski.sturtle.core.{TurtleCommand, TurtleQuery, TurtleQueryAnswer}
import com.github.scytrowski.sturtle.geometry.Angle
import com.github.scytrowski.sturtle.remoting.fixture.EffectSpecLike
import com.github.scytrowski.sturtle.remoting.mock.{TestTurtleClientJobRegistry, TestTurtleClientSideClient}
import com.github.scytrowski.sturtle.remoting.protocol.TurtleServerCommand

class AcquiredTurtleClientHandlerTest extends EffectSpecLike {
  "AcquiredTurtleClientHandler" when {

    "run" should {

      "run command" in {
        val command = TurtleCommand.Fill
        val io =
          for {
            registryData <- Ref.of[IO, TestTurtleClientJobRegistry.Data](TestTurtleClientJobRegistry.Data(
              results = List(TurtleClientOperationResult.CommandRan)
            ))
            clientData   <- Ref.of[IO, TestTurtleClientSideClient.Data](TestTurtleClientSideClient.Data())
            registry     = new TestTurtleClientJobRegistry(registryData)
            client       = new TestTurtleClientSideClient(clientData)
            rawHandler   = new RawTurtleClientHandler(client, registry)
            handler      = new AcquiredTurtleClientHandler(rawHandler)
            _            <- handler.run(command)
            cd           <- clientData.get
          } yield cd
        val clientData = io.runTimed()

        clientData.out.loneElement mustBe TurtleServerCommand.RunCommand(command)
      }

      "fail on unexpected result" in {
        val command = TurtleCommand.Fill
        val io =
          for {
            registryData <- Ref.of[IO, TestTurtleClientJobRegistry.Data](TestTurtleClientJobRegistry.Data(
              results = List(TurtleClientOperationResult.TurtleReleased)
            ))
            clientData   <- Ref.of[IO, TestTurtleClientSideClient.Data](TestTurtleClientSideClient.Data())
            registry     = new TestTurtleClientJobRegistry(registryData)
            client       = new TestTurtleClientSideClient(clientData)
            rawHandler   = new RawTurtleClientHandler(client, registry)
            handler      = new AcquiredTurtleClientHandler(rawHandler)
            _            <- handler.run(command)
          } yield ()
        val result = io.attempt.runTimed()

        result must be ('left)
      }

    }

    "execute" should {

      "execute query" in {
        val query = TurtleQuery.GetAngle
        val answer = TurtleQueryAnswer.AngleAnswer(Angle.radians(1.23))
        val io =
          for {
            registryData <- Ref.of[IO, TestTurtleClientJobRegistry.Data](TestTurtleClientJobRegistry.Data(
              results = List(TurtleClientOperationResult.QueryExecuted(answer))
            ))
            clientData   <- Ref.of[IO, TestTurtleClientSideClient.Data](TestTurtleClientSideClient.Data())
            registry     = new TestTurtleClientJobRegistry(registryData)
            client       = new TestTurtleClientSideClient(clientData)
            rawHandler   = new RawTurtleClientHandler(client, registry)
            handler      = new AcquiredTurtleClientHandler(rawHandler)
            actualAnswer <- handler.execute(query)
            cd           <- clientData.get
          } yield actualAnswer -> cd
        val (actualAnswer, clientData) = io.runTimed()

        actualAnswer mustBe answer
        clientData.out.loneElement mustBe TurtleServerCommand.ExecuteQuery(query)
      }

      "fail on unexpected result" in {
        val io =
          for {
            registryData <- Ref.of[IO, TestTurtleClientJobRegistry.Data](TestTurtleClientJobRegistry.Data(
              results = List(TurtleClientOperationResult.TurtleSelected)
            ))
            clientData   <- Ref.of[IO, TestTurtleClientSideClient.Data](TestTurtleClientSideClient.Data())
            registry     = new TestTurtleClientJobRegistry(registryData)
            client       = new TestTurtleClientSideClient(clientData)
            rawHandler   = new RawTurtleClientHandler(client, registry)
            handler      = new AcquiredTurtleClientHandler(rawHandler)
            _            <- handler.execute(TurtleQuery.GetPenColor)
          } yield ()
        val result = io.attempt.runTimed()

        result must be ('left)
      }

    }

  }
}
