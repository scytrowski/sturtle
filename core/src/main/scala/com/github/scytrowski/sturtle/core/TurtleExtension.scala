package com.github.scytrowski.sturtle.core

import cats.effect.Resource

trait TurtleExtension[F[_]] {
  def eventSourcing(description: TurtleEventSourcingDescription[F]): Resource[F, TurtleEventSourcing[F]]
}
