package com.github.scytrowski.sturtle.core

import cats.Applicative
import com.github.scytrowski.sturtle.es.CommandHandler
import com.github.scytrowski.sturtle.geometry.{Angle, Point, Vector}

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
}