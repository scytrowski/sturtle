package com.github.scytrowski.sturtle.turtle

import cats.effect.concurrent.Ref
import cats.syntax.functor._
import cats.syntax.flatMap._
import com.github.scytrowski.sturtle.fixture.CommonSpecLike
import com.github.scytrowski.sturtle.geometry.Point
import com.github.scytrowski.sturtle.mock.TestTurtleEventSourcing
import com.github.scytrowski.sturtle.turtle.TurtleCommand.MoveTo

class TurtleControllerTest extends CommonSpecLike {
  "TurtleController" when {

    "run" should {

      "handle command on event sourcing" in {
        val command = MoveTo(Point.cartesian(5, 6))

        val commands = for {
          commands   <- Ref.of(List.empty[TurtleCommand])
          es         = TestTurtleEventSourcing(commands)
          controller = TurtleController("123", Turtle.initial, es)
          _          <- controller.run(command)
          c          <- commands.get
        } yield c

        commands mustBe List(command)
      }

    }

  }
}
