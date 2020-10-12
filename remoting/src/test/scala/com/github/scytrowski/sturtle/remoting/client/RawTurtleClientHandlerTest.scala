package com.github.scytrowski.sturtle.remoting.client

import cats.effect.IO
import cats.effect.concurrent.Ref
import com.github.scytrowski.sturtle.remoting.fixture.EffectSpecLike
import com.github.scytrowski.sturtle.remoting.mock.{TestTurtleClientJobRegistry, TestTurtleClientSideClient}
import com.github.scytrowski.sturtle.remoting.protocol.{TurtleServerCommand, TurtleClientCommand}

class RawTurtleClientHandlerTest extends EffectSpecLike {
  "RawTurtleClientHandler" when {

    "executeCommand" should {

      "register job and send command via client" in {
        val command = TurtleServerCommand.SelectTurtle("select")
        val io =
          for {
            registryData <- Ref.of[IO, TestTurtleClientJobRegistry.Data](TestTurtleClientJobRegistry.Data())
            clientData   <- Ref.of[IO, TestTurtleClientSideClient.Data](TestTurtleClientSideClient.Data())
            registry = new TestTurtleClientJobRegistry(registryData)
            client   = new TestTurtleClientSideClient(clientData)
            handler  = new RawTurtleClientHandler(client, registry)
            _        <- handler.executeCommand(command)
            rd       <- registryData.get
            cd       <- clientData.get
          } yield rd -> cd
        val (registryData, clientData) = io.runTimed()

        registryData.register mustBe 1
        clientData.out.loneElement mustBe command
      }

    }

  }
}
