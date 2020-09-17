package com.github.scytrowski.sturtle.core

import com.github.scytrowski.sturtle.core.TurtleCommand.{MoveBackward, MoveBy, MoveForward, MoveTo, RotateLeftBy, RotateRightBy, RotateTo}
import com.github.scytrowski.sturtle.geometry.{Angle, Point, Vector}

trait TurtleController[F[_]] {
  def run(command: TurtleCommand): F[Unit]

  final def moveTo(point: Point): F[Unit] = run(MoveTo(point))

  final def moveBy(vector: Vector): F[Unit] = run(MoveBy(vector))

  final def moveForward(radius: Double): F[Unit] = run(MoveForward(radius))

  final def moveBackward(radius: Double): F[Unit] = run(MoveBackward(radius))

  final def rotateTo(angle: Angle): F[Unit] = run(RotateTo(angle))

  final def rotateLeftBy(angle: Angle): F[Unit] = run(RotateLeftBy(angle))

  final def rotateRightBy(angle: Angle): F[Unit] = run(RotateRightBy(angle))
}
