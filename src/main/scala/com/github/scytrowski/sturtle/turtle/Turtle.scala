package com.github.scytrowski.sturtle.turtle

import com.github.scytrowski.sturtle.geometry.{Angle, Point}

final case class Turtle(position: Point,
                        angle: Angle)

object Turtle {
  val initial: Turtle = Turtle(Point.zero, Angle.zero)
}
