package com.github.scytrowski.sturtle.remoting.client

import cats.effect.IO
import cats.effect.concurrent.Ref
import com.github.scytrowski.sturtle.remoting.fixture.EffectSpecLike
import com.github.scytrowski.sturtle.remoting.mock.{TestTurtleClientJobRegistry, TestTurtleClientSideClient}
import com.github.scytrowski.sturtle.remoting.protocol.TurtleServerCommand

class TurtleClientHandlerTest extends EffectSpecLike {
  "TurtleClientHandler" when {

    "use" should {

      "select and release turtle by provided id" in {
        val turtleId = "test-turtle"
        val io =
          for {
            registryData <- Ref.of[IO, TestTurtleClientJobRegistry.Data](TestTurtleClientJobRegistry.Data(
              results = List(TurtleClientOperationResult.TurtleSelected, TurtleClientOperationResult.TurtleReleased)
            ))
            clientData   <- Ref.of[IO, TestTurtleClientSideClient.Data](TestTurtleClientSideClient.Data())
            registry     = new TestTurtleClientJobRegistry(registryData)
            client       = new TestTurtleClientSideClient(clientData)
            rawHandler   = new RawTurtleClientHandler(client, registry)
            handler      = new TurtleClientHandler(rawHandler)
            _            <- handler.use(turtleId).use(_ => IO.unit)
            cd           <- clientData.get
          } yield cd
        val clientData = io.runTimed()

        clientData.out must contain theSameElementsInOrderAs List(
          TurtleServerCommand.SelectTurtle(turtleId),
          TurtleServerCommand.ReleaseTurtle
        )
      }

      "fail" when {

        "select executed with unexpected result" in {
          val io =
            for {
              registryData <- Ref.of[IO, TestTurtleClientJobRegistry.Data](TestTurtleClientJobRegistry.Data(
                results = List(TurtleClientOperationResult.CommandRan)
              ))
              clientData   <- Ref.of[IO, TestTurtleClientSideClient.Data](TestTurtleClientSideClient.Data())
              registry     = new TestTurtleClientJobRegistry(registryData)
              client       = new TestTurtleClientSideClient(clientData)
              rawHandler   = new RawTurtleClientHandler(client, registry)
              handler      = new TurtleClientHandler(rawHandler)
              _            <- handler.use("test-turtle").use(_ => IO.unit)
              cd           <- clientData.get
            } yield cd
          val result = io.attempt.runTimed()

          result must be ('left)
        }

        "release executed with unexpected result" in {
          val io =
            for {
              registryData <- Ref.of[IO, TestTurtleClientJobRegistry.Data](TestTurtleClientJobRegistry.Data(
                results = List(TurtleClientOperationResult.TurtleSelected, TurtleClientOperationResult.CommandRan)
              ))
              clientData   <- Ref.of[IO, TestTurtleClientSideClient.Data](TestTurtleClientSideClient.Data())
              registry     = new TestTurtleClientJobRegistry(registryData)
              client       = new TestTurtleClientSideClient(clientData)
              rawHandler   = new RawTurtleClientHandler(client, registry)
              handler      = new TurtleClientHandler(rawHandler)
              _            <- handler.use("test-turtle").use(_ => IO.unit)
              cd           <- clientData.get
            } yield cd
          val result = io.attempt.runTimed()

          result must be ('left)
        }

      }

    }

  }
}
