package com.github.scytrowski.sturtle.core

import cats.Id
import com.github.scytrowski.sturtle.core.TurtleQuery.{GetAngle, GetFillColor, GetPath, GetPenColor, GetPenState, GetPosition}
import com.github.scytrowski.sturtle.core.fixture.CommonSpecLike
import com.github.scytrowski.sturtle.geometry.{Angle, Path, Point}
import com.github.scytrowski.sturtle.graphics.Color

class TurtleQueryTest extends CommonSpecLike {
  "TurtleQuery" when {

    "handle" should {

      "handle GetPosition correctly" in {
        val position = Point.cartesian(5, 10)
        val turtle = Turtle.initial.copy(position = position)

        val result = handle(GetPosition, turtle)

        result mustBe position
      }

      "handle GetAngle correctly" in {
        val angle = Angle.radians(1.23)
        val turtle = Turtle.initial.copy(angle = angle)

        val result = handle(GetAngle, turtle)

        result mustBe angle
      }

      "handle GetPath correctly" in {
        val path = Path.empty ~> Point.zero
        val turtle = Turtle.initial.copy(path = path)

        val result = handle(GetPath, turtle)

        result mustBe path
      }

      "handle GetPenState correctly" in {
        val penState = PenState.Up
        val turtle = Turtle.initial.copy(penState = penState)

        val result = handle(GetPenState, turtle)

        result mustBe penState
      }

      "handle GetPenColor correctly" in {
        val penColor = Color.rgb(4, 9, 16)
        val turtle = Turtle.initial.copy(penColor = penColor)

        val result = handle(GetPenColor, turtle)

        result mustBe penColor
      }

      "handle GetFillColor correctly" in {
        val fillColor = Color.rgb(36, 49, 64)
        val turtle = Turtle.initial.copy(fillColor = fillColor)

        val result = handle(GetFillColor, turtle)

        result mustBe fillColor
      }

    }

  }

  private def handle(query: TurtleQuery, turtle: Turtle): query.Answer =
    TurtleQuery.handler[Id].handle(turtle, query)
}
