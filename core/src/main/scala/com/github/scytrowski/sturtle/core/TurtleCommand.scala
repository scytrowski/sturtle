package com.github.scytrowski.sturtle.core

import cats.Applicative
import com.github.scytrowski.sturtle.es.CommandHandler
import com.github.scytrowski.sturtle.geometry.{Angle, Point, Vector}
import com.github.scytrowski.sturtle.graphics.Color

sealed abstract class TurtleCommand

object TurtleCommand {
  def handler[F[_]: Applicative]: CommandHandler[TurtleCommand, TurtleEvent, F] =
    CommandHandler { cmd =>
      Applicative[F].pure {
        List {
          cmd match {
            case MoveTo(position) => TurtleEvent.MovedTo(position)
            case MoveBy(vector) => TurtleEvent.MovedBy(vector)
            case MoveForward(radius) => TurtleEvent.MovedForward(radius)
            case MoveBackward(radius) => TurtleEvent.MovedBackward(radius)
            case RotateTo(angle) => TurtleEvent.RotatedTo(angle)
            case RotateLeftBy(angle) => TurtleEvent.RotatedLeftBy(angle)
            case RotateRightBy(angle) => TurtleEvent.RotatedRightBy(angle)
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