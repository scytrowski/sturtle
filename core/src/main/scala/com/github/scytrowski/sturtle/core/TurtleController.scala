package com.github.scytrowski.sturtle.core

trait TurtleController[F[_]] {
  def run(command: TurtleCommand): F[Unit]
}
