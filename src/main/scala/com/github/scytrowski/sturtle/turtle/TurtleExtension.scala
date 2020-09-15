package com.github.scytrowski.sturtle.turtle

import cats.effect.Resource

trait TurtleExtension {
  def eventSourcingFactory[F[_]]: TurtleEventSourcingDescription[F] => Resource[F, TurtleEventSourcing[F]]
}
