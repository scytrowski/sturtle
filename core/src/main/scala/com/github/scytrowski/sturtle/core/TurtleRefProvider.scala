package com.github.scytrowski.sturtle.core

import cats.Monad

trait TurtleRefProvider[F[_]] {
  def ref(id: String): TurtleRef[F]
}

final class LocalTurtleRefProvider[F[_]: Monad](eventSourcing: TurtleEventSourcing[F]) extends TurtleRefProvider[F] {
  override def ref(id: String): TurtleRef[F] = new LocalTurtleRef(id, eventSourcing)
}
