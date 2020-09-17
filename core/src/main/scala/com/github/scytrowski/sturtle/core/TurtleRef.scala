package com.github.scytrowski.sturtle.core

import cats.effect.Resource

trait TurtleRef[F[_]] {
  def id: String

  def controller: Resource[F, TurtleController[F]]
}
