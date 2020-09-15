package com.github.scytrowski.sturtle.geometry

import com.github.scytrowski.sturtle.fixture.CommonSpecLike

class PointTest extends CommonSpecLike {
  "Point" when {

    "cartesian" should {

      "create correct point" in {
        val x = 1.2
        val y = 3.4

        val point = Point.cartesian(x, y)

        point.x mustBe x
        point.y mustBe y
      }

    }

    "polar" should {

      "create correct point" in {
        val radius = 1.23
        val angle = Angle.radians(0.25)
        val center = Point.cartesian(-1, 5)

        val point = Point.polar(radius, angle, center)

        point.x mustBe radius * angle.cos + center.x
        point.y mustBe radius * angle.sin + center.y
      }

    }

    "plus" should {

      "yield correct result" in {
        val point = Point.cartesian(1, 2)
        val vector = Vector.cartesian(3, 4)

        val result = point.plus(vector)

        result mustBe Point.cartesian(point.x + vector.dx, point.y + vector.dy)
      }

    }

    "minus" should {

      "yield correct result" in {
        val point = Point.cartesian(1, 2)
        val vector = Vector.cartesian(3, 4)

        val result = point.minus(vector)

        result mustBe Point.cartesian(point.x - vector.dx, point.y - vector.dy)
      }

    }

  }
}
