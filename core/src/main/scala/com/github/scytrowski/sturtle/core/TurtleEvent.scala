package com.github.scytrowski.sturtle.core

import cats.Applicative
import com.github.scytrowski.sturtle.es.EventHandler
import com.github.scytrowski.sturtle.geometry.{Angle, Point, Vector}

sealed abstract class TurtleEvent

object TurtleEvent {
  def handler[F[_]: Applicative]: EventHandler[Turtle, TurtleEvent, F] =
    EventHandler { case (turtle, event) =>
      Applicative[F].pure {
        event match {
          case MovedTo(position) =>
            val angle = Angle.between(turtle.position, position)
            turtle.copy(position = position, angle = angle)
          case MovedBy(vector) =>
            val position = turtle.position + vector
            val angle = Angle.between(turtle.position, position)
            turtle.copy(position = position, angle = angle)
          case MovedForward(radius) =>
            val position = turtle.position + Vector.polar(radius, turtle.angle)
            turtle.copy(position = position)
          case MovedBackward(radius) =>
            val position = turtle.position - Vector.polar(radius, turtle.angle)
            turtle.copy(position = position)
          case RotatedTo(angle) =>
            turtle.copy(angle = angle)
          case RotatedLeftBy(angle) =>
            val newAngle = turtle.angle + angle
            turtle.copy(angle = newAngle)
          case RotatedRightBy(angle) =>
            val newAngle = turtle.angle - angle
            turtle.copy(angle = newAngle)
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
}
