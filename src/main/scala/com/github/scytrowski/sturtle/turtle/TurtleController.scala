package com.github.scytrowski.sturtle.turtle

import cats.Monad
import cats.effect.Sync
import cats.effect.concurrent.Ref
import com.github.scytrowski.sturtle.geometry.{Angle, Point, Vector}
import cats.syntax.flatMap._
import cats.syntax.functor._
import com.github.scytrowski.sturtle.turtle.TurtleCommand.{MoveBackward, MoveBy, MoveForward, MoveTo, RotateLeftBy, RotateRightBy, RotateTo}

final class TurtleController[F[_]: Monad] private(val id: String,
                                                  turtleRef: Ref[F, Turtle],
                                                  eventSourcing: TurtleEventSourcing[F]) {
  def moveTo(point: Point): F[Unit] = run(MoveTo(point))

  def moveBy(vector: Vector): F[Unit] = run(MoveBy(vector))

  def moveForward(radius: Double): F[Unit] = run(MoveForward(radius))

  def moveBackward(radius: Double): F[Unit] = run(MoveBackward(radius))

  def rotateTo(angle: Angle): F[Unit] = run(RotateTo(angle))

  def rotateLeftBy(angle: Angle): F[Unit] = run(RotateLeftBy(angle))

  def rotateRightBy(angle: Angle): F[Unit] = run(RotateRightBy(angle))

  def run(command: TurtleCommand): F[Unit] =
    for {
      turtle        <- turtleRef.get
      updatedTurtle <- eventSourcing.run(turtle, command)
      _             <- turtleRef.set(updatedTurtle)
    } yield ()
}

object TurtleController {
  def apply[F[_]: Sync](id: String, turtle: Turtle, eventSourcing: TurtleEventSourcing[F]): F[TurtleController[F]] =
    Ref.of(turtle).map(new TurtleController[F](id, _, eventSourcing))
}
