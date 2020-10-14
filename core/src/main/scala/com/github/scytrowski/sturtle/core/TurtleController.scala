package com.github.scytrowski.sturtle.core

import cats.Monad

trait TurtleController[F[_]] {
  def run(command: TurtleCommand): F[Unit]

  def execute(query: TurtleQuery): F[TurtleQueryAnswer]
}

private[core] final class LocalTurtleController[F[_]: Monad](entity: TurtleEventSourcedEntity[F]) extends TurtleController[F] {
  override def run(command: TurtleCommand): F[Unit] = entity.run(command)

  override def execute(query: TurtleQuery): F[query.Answer] = entity.execute(query)
}
