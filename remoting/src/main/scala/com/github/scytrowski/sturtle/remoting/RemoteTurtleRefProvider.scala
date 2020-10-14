package com.github.scytrowski.sturtle.remoting

import cats.effect.{Concurrent, ContextShift}
import com.github.scytrowski.sturtle.core.{TurtleRef, TurtleRefProvider}

final class RemoteTurtleRefProvider[F[_]: Concurrent: ContextShift](connectionInfo: TurtleConnectionInfo) extends TurtleRefProvider[F] {
  override def ref(id: String): TurtleRef[F] = new RemoteTurtleRef(id, connectionInfo)
}
