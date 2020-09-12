package com.github.scytrowski.sturtle.turtle

import cats.Id
import com.github.scytrowski.sturtle.geometry.{Angle, Position, Vector}
import com.github.scytrowski.sturtle.turtle.TurtleCommand.{MoveBackward, MoveBy, MoveForward, MoveTo, RotateLeftBy, RotateRightBy, RotateTo}
import com.github.scytrowski.sturtle.turtle.TurtleEvent.{MovedBackward, MovedBy, MovedForward, MovedTo, RotatedLeftBy, RotatedRightBy, RotatedTo}
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class TurtleCommandTest extends AnyWordSpec with Matchers {
  private val position = Position(1, 2)
  private val vector = Vector(3, 4)
  private val angle = Angle(0.123)

  "TurtleCommand" when {

    "handle" should {

      "convert MoveTo correctly" in {
        convert(MoveTo(position)) mustBe List(MovedTo(position))
      }

      "convert MoveBy correctly" in {
        convert(MoveBy(vector)) mustBe List(MovedBy(vector))
      }

      "convert MoveForward correctly" in {
        convert(MoveForward(1234)) mustBe List(MovedForward(1234))
      }

      "convert MoveBackward correctly" in {
        convert(MoveBackward(4321)) mustBe List(MovedBackward(4321))
      }

      "convert RotateTo correctly" in {
        convert(RotateTo(angle)) mustBe List(RotatedTo(angle))
      }

      "convert RotateLeftBy correctly" in {
        convert(RotateLeftBy(angle)) mustBe List(RotatedLeftBy(angle))
      }

      "convert RotateRightBy correctly" in {
        convert(RotateRightBy(angle)) mustBe List(RotatedRightBy(angle))
      }

    }

  }

  private def convert(command: TurtleCommand): List[TurtleEvent] =
    TurtleCommand.handler[Id].handle(command)
}
