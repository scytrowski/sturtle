package com.github.scytrowski.sturtle.core.geometry

import com.github.scytrowski.sturtle.core.fixture.CommonSpecLike

class PathTest extends CommonSpecLike {
  "Path" when {

    "to" should {

      "add point to the path" in {
        val initialPath = Path.empty
        val point = Point.cartesian(1, 2)

        val path = initialPath.to(point)

        path.points mustBe List(point)
      }

    }

  }
}
