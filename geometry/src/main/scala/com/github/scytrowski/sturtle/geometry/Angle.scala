package com.github.scytrowski.sturtle.geometry

final case class Angle private(value: Double) {
  def negate: Angle = Angle.radians(-value)
  def unary_- : Angle = negate

  def plus(other: Angle): Angle = Angle.radians(value + other.value)
  def +(other: Angle): Angle = plus(other)

  def minus(other: Angle): Angle = Angle.radians(value - other.value)
  def -(other: Angle): Angle = minus(other)

  def times(v: Double): Angle = Angle.radians(value * v)
  def *(v: Double): Angle = times(v)

  def divide(v: Double): Angle = Angle.radians(value / v)
  def /(v: Double): Angle = divide(v)

  def sin: Double = Math.sin(value)
  def cos: Double = Math.cos(value)
}

object Angle {
  val zero: Angle = Angle(0)

  def radians(value: Double): Angle = Angle(normalizeValue(value))

  def between(p1: Point, p2: Point): Angle = {
    val vector = Vector.between(p1, p2)
    val value = Math.atan2(vector.dy, vector.dx)
    radians(value)
  }

  private def normalizeValue(value: Double): Double =
    if (value >= fullCircle)
      value - Math.floor(value / fullCircle) * fullCircle
    else if (value < 0)
      value + Math.ceil(Math.abs(value) / fullCircle) * fullCircle
    else
      value

  private val fullCircle = 2 * Math.PI
}
