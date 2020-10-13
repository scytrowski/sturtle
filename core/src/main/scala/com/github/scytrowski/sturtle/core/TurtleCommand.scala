package com.github.scytrowski.sturtle.core

import cats.Applicative
import com.github.scytrowski.sturtle.es.CommandHandler
import com.github.scytrowski.sturtle.geometry.{Angle, Point, Vector}
import com.github.scytrowski.sturtle.graphics.Color

sealed abstract class TurtleCommand

object TurtleCommand {
  def handler[F[_]: Applicative]: CommandHandler[F, Turtle, TurtleCommand, TurtleEvent] =
    CommandHandler { case (turtle, cmd) =>
      Applicative[F].pure {
        List {
          cmd match {
            case MoveTo(position) =>
              val vector = Vector.between(turtle.position, position)
              TurtleEvent.MovedBy(vector)
            case MoveBy(vector) => TurtleEvent.MovedBy(vector)
            case MoveForward(radius) =>
              val vector = Vector.polar(radius, turtle.angle)
              TurtleEvent.MovedBy(vector)
            case MoveBackward(radius) =>
              val vector = -Vector.polar(radius, turtle.angle)
              TurtleEvent.MovedBy(vector)
            case RotateTo(angle) =>
              val by = angle - turtle.angle
              TurtleEvent.RotatedBy(by)
            case RotateLeftBy(angle) =>
              TurtleEvent.RotatedBy(angle)
            case RotateRightBy(angle) =>
              TurtleEvent.RotatedBy(-angle)
            case Fill => TurtleEvent.Filled
            case ClearPath => TurtleEvent.ClearedPath
            case PenDown => TurtleEvent.SetPenDown
            case PenUp => TurtleEvent.SetPenUp
            case SetPenColor(color) => TurtleEvent.SetPenColorEvent(color)
            case SetFillColor(color) => TurtleEvent.SetFillColorEvent(color)
          }
        }
      }
    }

  final case class MoveTo(position: Point) extends TurtleCommand
  final case class MoveBy(vector: Vector) extends TurtleCommand
  final case class MoveForward(radius: Double) extends TurtleCommand
  final case class MoveBackward(radius: Double) extends TurtleCommand
  final case class RotateTo(angle: Angle) extends TurtleCommand
  final case class RotateLeftBy(angle: Angle) extends TurtleCommand
  final case class RotateRightBy(angle: Angle) extends TurtleCommand
  case object Fill extends TurtleCommand
  case object ClearPath extends TurtleCommand
  case object PenDown extends TurtleCommand
  case object PenUp extends TurtleCommand
  final case class SetPenColor(color: Color) extends TurtleCommand
  final case class SetFillColor(color: Color) extends TurtleCommand
}