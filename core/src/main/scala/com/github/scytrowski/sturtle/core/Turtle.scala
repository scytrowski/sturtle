package com.github.scytrowski.sturtle.core

import com.github.scytrowski.sturtle.core.geometry.{Angle, Path, Point}
import com.github.scytrowski.sturtle.core.graphics.Color

final case class Turtle(position: Point,
                        angle: Angle,
                        path: Path,
                        penState: PenState,
                        penColor: Color,
                        fillColor: Color)

object Turtle {
  val initial: Turtle = Turtle(Point.zero, Angle.zero, Path.empty, PenState.Down, Color.black, Color.black)
}
