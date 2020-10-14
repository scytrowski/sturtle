package com.github.scytrowski.sturtle.geometry

final case class Vector private(dx: Double, dy: Double) {
  def negate: Vector = Vector(-dx, -dy)
  def unary_- : Vector = negate

  def plus(other: Vector): Vector = Vector(dx + other.dx, dy + other.dy)
  def +(other: Vector): Vector = plus(other)

  def minus(other: Vector): Vector = Vector(dx - other.dx, dy - other.dy)
  def -(other: Vector): Vector = minus(other)

  def times(v: Double): Vector = Vector(dx * v, dy * v)
  def *(v: Double): Vector = times(v)

  def divide(v: Double): Vector = Vector(dx / v, dy / v)
  def /(v: Double): Vector = divide(v)
}

object Vector {
  val zero: Vector = Vector(0, 0)

  def cartesian(dx: Double, dy: Double): Vector = Vector(dx, dy)

  def polar(radius: Double, angle: Angle): Vector = {
    val dx = radius * angle.cos
    val dy = radius * angle.sin
    Vector.cartesian(dx, dy)
  }

  def between(p1: Point, p2: Point): Vector = Vector(p2.x - p1.x, p2.y - p1.y)
}
