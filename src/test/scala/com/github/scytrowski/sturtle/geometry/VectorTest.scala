package com.github.scytrowski.sturtle.geometry

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class VectorTest extends AnyWordSpec with Matchers {
  "Vector" when {

    "cartesian" should {

      "create correct vector" in {
        val dx = 3
        val dy = 4

        val vector = Vector.cartesian(dx, dy)

        vector.dx mustBe dx
        vector.dy mustBe dy
      }

    }

    "polar" should {

      "create correct vector" in {
        val radius = 1.234
        val angle = Angle.radians(0.333)

        val vector = Vector.polar(radius, angle)

        vector.dx mustBe radius * angle.cos
        vector.dy mustBe radius * angle.sin
      }

    }

    "plus" should {

      "yield correct result" in {
        val vector = Vector(1, 2)
        val other = Vector(3, 4)

        val result = vector.plus(other)

        result mustBe Vector.cartesian(vector.dx + other.dx, vector.dy + other.dy)
      }

    }

    "minus" should {

      "yield correct result" in {
        val vector = Vector(1, 2)
        val other = Vector(3, 4)

        val result = vector.minus(other)

        result mustBe Vector.cartesian(vector.dx - other.dx, vector.dy - other.dy)
      }

    }

    "times" should {

      "yield correct result" in {
        val vector = Vector(1, 2)
        val v = 3.456

        val result = vector.times(v)

        result mustBe Vector.cartesian(vector.dx * v, vector.dy * v)
      }

    }

    "divide" should {

      "yield correct result" in {
        val vector = Vector(1, 2)
        val v = 7.89

        val result = vector.divide(v)

        result mustBe Vector.cartesian(vector.dx / v, vector.dy / v)
      }

    }

  }
}
