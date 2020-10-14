package com.github.scytrowski.sturtle.core.graphics

import com.github.scytrowski.sturtle.core.fixture.CommonSpecLike

class ColorTest extends CommonSpecLike {
  "Color" when {

    "rgb" should {

      "normalize upper-bound component value" in {
        val color = Color.rgb(255, 111, 98, bitDepth = 7)

        color.red mustBe 127
        color.green mustBe 111
        color.blue mustBe 98
      }

      "normalize lower-bound component value" in {
        val color = Color.rgb(1, -5, 13)

        color.red mustBe 1
        color.green mustBe 0
        color.blue mustBe 13
      }

      "create color without normalization needed" in {
        val color = Color.rgb(149, 81, 27)

        color.red mustBe 149
        color.green mustBe 81
        color.blue mustBe 27
      }

    }

  }
}
