package com.github.scytrowski.sturtle.core

import com.github.scytrowski.sturtle.core.geometry.{Angle, Path, Point}
import com.github.scytrowski.sturtle.core.graphics.Color

sealed abstract class TurtleQueryAnswer

object TurtleQueryAnswer {
  final case class PositionAnswer(position: Point) extends TurtleQueryAnswer
  final case class AngleAnswer(angle: Angle) extends TurtleQueryAnswer
  final case class PathAnswer(path: Path) extends TurtleQueryAnswer
  final case class PenStateAnswer(penState: PenState) extends TurtleQueryAnswer
  final case class PenColorAnswer(penColor: Color) extends TurtleQueryAnswer
  final case class FillColorAnswer(fillColor: Color) extends TurtleQueryAnswer
}
