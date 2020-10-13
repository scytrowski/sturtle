package com.github.scytrowski.sturtle.core

import cats.Applicative
import com.github.scytrowski.sturtle.es.EventHandler
import com.github.scytrowski.sturtle.geometry.{Angle, Path, Vector}
import com.github.scytrowski.sturtle.graphics.Color

sealed abstract class TurtleEvent

object TurtleEvent {
  def handler[F[_]: Applicative]: EventHandler[F, Turtle, TurtleEvent] =
    EventHandler { case (turtle, event) =>
      Applicative[F].pure {
        event match {
          case MovedBy(vector) =>
            val position = turtle.position + vector
            val angle = Angle.between(turtle.position, position)
            val path = turtle.path ~> position
            turtle.copy(position = position, angle = angle, path = path)
          case RotatedBy(angle) =>
            turtle.copy(angle = turtle.angle + angle)
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

  final case class MovedBy(vector: Vector) extends TurtleEvent
  final case class RotatedBy(angle: Angle) extends TurtleEvent
  case object Filled extends TurtleEvent
  case object ClearedPath extends TurtleEvent
  case object SetPenDown extends TurtleEvent
  case object SetPenUp extends TurtleEvent
  final case class SetPenColorEvent(color: Color) extends TurtleEvent
  final case class SetFillColorEvent(color: Color) extends TurtleEvent
}
