package com.github.scytrowski.sturtle.turtle

import cats.effect.Resource

trait TurtleExtension {
  def eventSourcing[F[_]](description: TurtleEventSourcingDescription[F]): Resource[F, TurtleEventSourcing[F]]
}
