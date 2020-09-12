package com.github.scytrowski.sturtle.turtle

import com.github.scytrowski.sturtle.geometry.{Angle, Position}

final case class Turtle(position: Position,
                        angle: Angle)

object Turtle {
  val initial: Turtle = Turtle(Position.zero, Angle.zero)
}
