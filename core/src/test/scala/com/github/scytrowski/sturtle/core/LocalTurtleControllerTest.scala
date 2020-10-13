package com.github.scytrowski.sturtle.core

import cats.effect.IO
import cats.effect.concurrent.Ref
import com.github.scytrowski.sturtle.core.TurtleCommand.MoveTo
import com.github.scytrowski.sturtle.core.TurtleQuery.GetAngle
import com.github.scytrowski.sturtle.core.fixture.CommonSpecLike
import com.github.scytrowski.sturtle.core.mock.TestTurtleEventSourcing
import com.github.scytrowski.sturtle.geometry.Point

class LocalTurtleControllerTest extends CommonSpecLike {
  "LocalTurtleController" when {

    "run" should {

      "handle command on event sourcing" in {
        val command = MoveTo(Point.cartesian(5, 6))

        val io =
          for {
            data <- Ref.of[IO, TestTurtleEventSourcing.TestData](TestTurtleEventSourcing.TestData())
            es   = TestTurtleEventSourcing(data)
            ctrl <- LocalTurtleController("123", es)
            _    <- ctrl.run(command)
            d    <- data.get
          } yield d.commands
        val commands = io.unsafeRunSync()

        commands mustBe List(command)
      }

      "handle query on event sourcing" in {
        val query = GetAngle

        val io =
          for {
            data <- Ref.of[IO, TestTurtleEventSourcing.TestData](TestTurtleEventSourcing.TestData())
            es   = TestTurtleEventSourcing(data)
            ctrl <- LocalTurtleController("123", es)
            _    <- ctrl.execute(query)
            d    <- data.get
          } yield d.queries
        val queries = io.unsafeRunSync()

        queries mustBe List(query)
      }

    }

  }
}
