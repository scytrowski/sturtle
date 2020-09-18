package com.github.scytrowski.sturtle.core

import cats.Id
import com.github.scytrowski.sturtle.core.fixture.CommonSpecLike
import com.github.scytrowski.sturtle.core.TurtleEvent._
import com.github.scytrowski.sturtle.geometry.{Angle, Path, Point, Vector}
import com.github.scytrowski.sturtle.graphics.Color

class TurtleEventTest extends CommonSpecLike {
  "TurtleEvent" when {

    "handle" should {

      "handle MovedTo correctly" in {
        val point = Point.cartesian(5, 4)

        val turtle = handle(MovedTo(point))

        turtle.position mustBe point
        turtle.angle mustBe Angle.between(Point.zero, point)
      }

      "handle MovedBy correctly" in {
        val vector = Vector.cartesian(-3, 9)

        val turtle = handle(MovedBy(vector))

        val expectedPosition = Point.zero + vector
        turtle.position mustBe expectedPosition
        turtle.angle mustBe Angle.between(Point.zero, expectedPosition)
      }

      "handle MovedForward correctly" in {
        val radius = 123.456
        val angle = Angle.radians(1.591)
        val initialTurtle = Turtle.initial.copy(angle = angle)

        val turtle = handle(MovedForward(radius), initialTurtle)

        val expectedPosition = Point.zero + Vector.polar(radius, angle)
        turtle.position mustBe expectedPosition
      }

      "handle MovedBackward correctly" in {
        val radius = 654.321
        val angle = Angle.radians(2.917)
        val initialTurtle = Turtle.initial.copy(angle = angle)

        val turtle = handle(MovedBackward(radius), initialTurtle)

        val expectedPosition = Point.zero - Vector.polar(radius, angle)
        turtle.position mustBe expectedPosition
      }

      "handle RotatedTo correctly" in {
        val angle = Angle.radians(3.583)

        val turtle = handle(RotatedTo(angle))

        turtle.angle mustBe angle
      }

      "handle RotatedLeftBy correctly" in {
        val initialAngle = Angle.radians(1.23)
        val angle = Angle.radians(4.972)
        val initialTurtle = Turtle.initial.copy(angle = initialAngle)

        val turtle = handle(RotatedLeftBy(angle), initialTurtle)

        turtle.angle mustBe initialAngle + angle
      }

      "handle RotatedRightBy correctly" in {
        val initialAngle = Angle.radians(3.21)
        val angle = Angle.radians(2.525)
        val initialTurtle = Turtle.initial.copy(angle = initialAngle)

        val turtle = handle(RotatedRightBy(angle), initialTurtle)

        turtle.angle mustBe initialAngle - angle
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
