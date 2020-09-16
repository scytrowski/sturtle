package com.github.scytrowski.sturtle.turtle

import cats.effect.{Resource, Sync}

trait TurtleRef[F[_]] {
  def id: String

  def controller: Resource[F, TurtleController[F]]
}
