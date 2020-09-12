package com.github.scytrowski.sturtle.turtle

import cats.Applicative
import com.github.scytrowski.sturtle.es.EventHandler
import com.github.scytrowski.sturtle.geometry.{Angle, Position, Vector}

sealed abstract class TurtleEvent

object TurtleEvent {
  def handler[F[_]: Applicative]: EventHandler[Turtle, TurtleEvent, F] = ???

  final case class MovedTo(position: Position) extends TurtleEvent
  final case class MovedBy(vector: Vector) extends TurtleEvent
  final case class MovedForward(radius: Double) extends TurtleEvent
  final case class MovedBackward(radius: Double) extends TurtleEvent
  final case class RotatedTo(angle: Angle) extends TurtleEvent
  final case class RotatedLeftBy(angle: Angle) extends TurtleEvent
  final case class RotatedRightBy(angle: Angle) extends TurtleEvent
}
