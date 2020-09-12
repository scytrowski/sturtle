package com.github.scytrowski.sturtle.geometry

final case class Vector(dx: Double, dy: Double)

object Vector {
  val zero: Vector = Vector(0, 0)
}
