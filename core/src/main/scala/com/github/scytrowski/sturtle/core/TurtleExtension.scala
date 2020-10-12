package com.github.scytrowski.sturtle.core

import cats.effect.Resource

trait TurtleExtension[F[_]] {
  def resource(manager: TurtleManager[F]): Resource[F, Unit]

  def eventSourcing(description: TurtleEventSourcingDescription[F]): Resource[F, TurtleEventSourcing[F]]
}
