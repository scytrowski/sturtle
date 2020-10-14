package com.github.scytrowski.sturtle.core

import cats.Id
import com.github.scytrowski.sturtle.core.TurtleCommand._
import com.github.scytrowski.sturtle.core.TurtleEvent._
import com.github.scytrowski.sturtle.core.fixture.CommonSpecLike
import com.github.scytrowski.sturtle.core.geometry.{Angle, Point, Vector}
import com.github.scytrowski.sturtle.core.graphics.Color

class TurtleCommandTest extends CommonSpecLike {
  private val position = Point.cartesian(1, 2)
  private val vector = Vector.cartesian(3, 4)
  private val angle = Angle.radians(0.123)
  private val color = Color.rgb(127, 15, 13)

  "TurtleCommand" when {

    "handle" should {

      "convert MoveTo correctly" in {
        convert(MoveTo(position)) mustBe List(MovedBy(Vector.between(Point.zero, position)))
      }

      "convert MoveBy correctly" in {
        convert(MoveBy(vector)) mustBe List(MovedBy(vector))
      }

      "convert MoveForward correctly" in {
        convert(MoveForward(1234)) mustBe List(MovedBy(Vector.polar(1234, Angle.zero)))
      }

      "convert MoveBackward correctly" in {
        convert(MoveBackward(4321)) mustBe List(MovedBy(-Vector.polar(4321, Angle.zero)))
      }

      "convert RotateTo correctly" in {
        convert(RotateTo(angle)) mustBe List(RotatedBy(angle))
      }

      "convert RotateLeftBy correctly" in {
        convert(RotateLeftBy(angle)) mustBe List(RotatedBy(angle))
      }

      "convert RotateRightBy correctly" in {
        convert(RotateRightBy(angle)) mustBe List(RotatedBy(-angle))
      }

      "convert Fill correctly" in {
        convert(Fill) mustBe List(Filled)
      }

      "convert ClearPath correctly" in {
        convert(ClearPath) mustBe List(ClearedPath)
      }

      "convert PenDown correctly" in {
        convert(PenDown) mustBe List(SetPenDown)
      }

      "convert PenUp correctly" in {
        convert(PenUp) mustBe List(SetPenUp)
      }

      "convert SetPenColor correctly" in {
        convert(SetPenColor(color)) mustBe List(SetPenColorEvent(color))
      }

      "convert SetFillColor correctly" in {
        convert(SetFillColor(color)) mustBe List(SetFillColorEvent(color))
      }

    }

  }

  private def convert(command: TurtleCommand): List[TurtleEvent] =
    TurtleCommand.handler[Id].handle(Turtle.initial, command)
}
