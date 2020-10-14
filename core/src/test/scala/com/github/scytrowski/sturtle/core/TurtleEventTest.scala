package com.github.scytrowski.sturtle.core

import cats.Id
import com.github.scytrowski.sturtle.core.fixture.CommonSpecLike
import com.github.scytrowski.sturtle.core.TurtleEvent._
import com.github.scytrowski.sturtle.core.geometry.{Angle, Path, Point, Vector}
import com.github.scytrowski.sturtle.core.graphics.Color

class TurtleEventTest extends CommonSpecLike {
  "TurtleEvent" when {

    "handle" should {

      "handle MovedBy correctly" in {
        val vector = Vector.cartesian(-3, 9)

        val turtle = handle(MovedBy(vector))

        val expectedPosition = Point.zero + vector
        turtle.position mustBe expectedPosition
        turtle.angle mustBe Angle.between(Point.zero, expectedPosition)
      }

      "handle RotatedBy correctly" in {
        val angle = Angle.radians(1.23)

        val turtle = handle(RotatedBy(angle))

        val expectedAngle = Angle.zero + angle
        turtle.angle mustBe expectedAngle
      }

      "handle Filled correctly" in {
        val initialTurtle = Turtle.initial.copy(path = Path.empty ~> Point.zero)

        val turtle = handle(Filled, initialTurtle)

        turtle.path mustBe Path.empty
      }

      "handle ClearedPath correctly" in {
        val initialTurtle = Turtle.initial.copy(path = Path.empty ~> Point.zero)

        val turtle = handle(ClearedPath, initialTurtle)

        turtle.path mustBe Path.empty
      }

      "handle SetPenDown correctly" in {
        val initialTurtle = Turtle.initial.copy(penState = PenState.Up)

        val turtle = handle(SetPenDown, initialTurtle)

        turtle.penState mustBe PenState.Down
      }

      "handle SetPenUp correctly" in {
        val turtle = handle(SetPenUp)

        turtle.penState mustBe PenState.Up
      }

      "handle SetPenColorEvent correctly" in {
        val color = Color.rgb(13, 14, 15)

        val turtle = handle(SetPenColorEvent(color))

        turtle.penColor mustBe color
      }

      "handle SetFillColorEvent correctly" in {
        val color = Color.rgb(16, 17, 18)

        val turtle = handle(SetFillColorEvent(color))

        turtle.fillColor mustBe color
      }

    }

  }

  private def handle(event: TurtleEvent, turtle: Turtle = Turtle.initial): Turtle =
    TurtleEvent.handler[Id].handle(turtle, event)
}
