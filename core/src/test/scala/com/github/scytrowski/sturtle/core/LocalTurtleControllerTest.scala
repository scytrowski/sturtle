package com.github.scytrowski.sturtle.core

import cats.effect.concurrent.Ref
import cats.syntax.functor._
import cats.syntax.flatMap._
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

        val commands =
          for {
            data <- Ref.of(TestTurtleEventSourcing.TestData())
            es   = TestTurtleEventSourcing(data)
            ctrl <- LocalTurtleController("123", es)
            _    <- ctrl.run(command)
            d    <- data.get
          } yield d.commands

        commands mustBe List(command)
      }

      "handle query on event sourcing" in {
        val query = GetAngle

        val queries =
          for {
            data <- Ref.of(TestTurtleEventSourcing.TestData())
            es   = TestTurtleEventSourcing(data)
            ctrl <- LocalTurtleController("123", es)
            _    <- ctrl.execute(query)
            d    <- data.get
          } yield d.queries

        queries mustBe List(query)
      }

    }

  }
}
