package com.github.scytrowski.sturtle.core.geometry

final case class Point private(x: Double, y: Double) {
  def plus(vector: Vector): Point = Point(x + vector.dx, y + vector.dy)
  def +(vector: Vector): Point = plus(vector)

  def minus(vector: Vector): Point = Point(x - vector.dx, y - vector.dy)
  def -(vector: Vector): Point = minus(vector)
}

object Point {
  val zero: Point = Point(0, 0)

  def cartesian(x: Double, y: Double): Point = Point(x, y)

  def polar(radius: Double, angle: Angle, center: Point = Point.zero): Point = {
    val x = radius * angle.cos + center.x
    val y = radius * angle.sin + center.y
    Point.cartesian(x, y)
  }
}
