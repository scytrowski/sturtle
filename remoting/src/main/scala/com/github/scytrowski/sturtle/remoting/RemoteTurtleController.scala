package com.github.scytrowski.sturtle.remoting

import com.github.scytrowski.sturtle.core.{TurtleCommand, TurtleController, TurtleQuery, TurtleQueryAnswer}
import com.github.scytrowski.sturtle.remoting.client.AcquiredTurtleClientHandler

private[remoting] final class RemoteTurtleController[F[_]](handler: AcquiredTurtleClientHandler[F]) extends TurtleController[F] {
  override def run(command: TurtleCommand): F[Unit] = handler.run(command)

  override def execute(query: TurtleQuery): F[TurtleQueryAnswer] = handler.execute(query)
}
