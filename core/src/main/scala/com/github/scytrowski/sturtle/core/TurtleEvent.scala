package com.github.scytrowski.sturtle.core

import cats.Applicative
import com.github.scytrowski.sturtle.es.EventHandler
import com.github.scytrowski.sturtle.geometry.{Angle, Path, Point, Vector}
import com.github.scytrowski.sturtle.graphics.Color

sealed abstract class TurtleEvent

object TurtleEvent {
  def handler[F[_]: Applicative]: EventHandler[Turtle, TurtleEvent, F] =
    EventHandler { case (turtle, event) =>
      Applicative[F].pure {
        event match {
          case MovedTo(position) =>
            val angle = Angle.between(turtle.position, position)
            val path = turtle.path ~> position
            turtle.copy(position = position, angle = angle, path = path)
          case MovedBy(vector) =>
            val position = turtle.position + vector
            val angle = Angle.between(turtle.position, position)
            val path = turtle.path ~> position
            turtle.copy(position = position, angle = angle, path = path)
          case MovedForward(radius) =>
            val position = turtle.position + Vector.polar(radius, turtle.angle)
            val path = turtle.path ~> position
            turtle.copy(position = position, path = path)
          case MovedBackward(radius) =>
            val position = turtle.position - Vector.polar(radius, turtle.angle)
            val path = turtle.path ~> position
            turtle.copy(position = position, path = path)
          case RotatedTo(angle) =>
            turtle.copy(angle = angle)
          case RotatedLeftBy(angle) =>
            val newAngle = turtle.angle + angle
            turtle.copy(angle = newAngle)
          case RotatedRightBy(angle) =>
            val newAngle = turtle.angle - angle
            turtle.copy(angle = newAngle)
          case Filled =>
            val path = Path.empty
            turtle.copy(path = path)
          case ClearedPath =>
            val path = Path.empty
            turtle.copy(path = path)
          case SetPenDown =>
            turtle.copy(penState = PenState.Down)
          case SetPenUp =>
            turtle.copy(penState = PenState.Up)
          case SetPenColorEvent(color) =>
            turtle.copy(penColor = color)
          case SetFillColorEvent(color) =>
            turtle.copy(fillColor = color)
        }
      }
    }

  final case class MovedTo(position: Point) extends TurtleEvent
  final case class MovedBy(vector: Vector) extends TurtleEvent
  final case class MovedForward(radius: Double) extends TurtleEvent
  final case class MovedBackward(radius: Double) extends TurtleEvent
  final case class RotatedTo(angle: Angle) extends TurtleEvent
  final case class RotatedLeftBy(angle: Angle) extends TurtleEvent
  final case class RotatedRightBy(angle: Angle) extends TurtleEvent
  case object Filled extends TurtleEvent
  case object ClearedPath extends TurtleEvent
  case object SetPenDown extends TurtleEvent
  case object SetPenUp extends TurtleEvent
  final case class SetPenColorEvent(color: Color) extends TurtleEvent
  final case class SetFillColorEvent(color: Color) extends TurtleEvent
}
