package com.github.scytrowski.sturtle.geometry

import com.github.scytrowski.sturtle.geometry.fixture.CommonSpecLike

class AngleTest extends CommonSpecLike {
  "Angle" when {

    "radians" should {

      "create correct angle" when {

        "value is already normalized" in {
          val value = 3.467

          val angle = Angle.radians(value)

          angle.value mustBe value
        }

        "value must be upper-bound normalized" in {
          val value = 5.421

          val angle = Angle.radians(value + 6 * Math.PI)

          angle.value mustBe value +- 0.1
        }

        "value must be lower-bound normalized" in {
          val value = 4.321

          val angle = Angle.radians(value - 8 * Math.PI)

          angle.value mustBe value +- 0.1
        }

      }

    }

    "plus" should {

      "yield correct result" in {
        val angle = Angle.radians(1.234)
        val other = Angle.radians(3.456)

        val result = angle.plus(other)

        result.value mustBe angle.value + other.value
      }

    }

    "minus" should {

      "yield correct result" in {
        val angle = Angle.radians(3.456)
        val other = Angle.radians(1.234)

        val result = angle.minus(other)

        result.value mustBe angle.value - other.value
      }

    }

    "times" should {

      "yield correct result" in {
        val angle = Angle.radians(1.234)
        val v = 3.5

        val result = angle.times(v)

        result.value mustBe angle.value * v
      }

    }

    "divide" should {

      "yield correct result" in {
        val angle = Angle.radians(1.234)
        val v = 0.75

        val result = angle.divide(v)

        result.value mustBe angle.value / v
      }

    }

  }
}
