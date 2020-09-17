package com.github.scytrowski.sturtle.core

import cats.Monad
import cats.syntax.functor._
import cats.syntax.flatMap._
import cats.effect.Sync
import cats.effect.concurrent.Ref

final class LocalTurtleController[F[_]: Monad] private(val id: String,
                                                       turtleRef: Ref[F, Turtle],
                                                       eventSourcing: TurtleEventSourcing[F]) extends TurtleController[F] {
  override def run(command: TurtleCommand): F[Unit] =
    for {
      turtle        <- turtleRef.get
      updatedTurtle <- eventSourcing.run(turtle, command)
      _             <- turtleRef.set(updatedTurtle)
    } yield ()
}

object LocalTurtleController {
  def apply[F[_]: Sync](id: String, eventSourcing: TurtleEventSourcing[F]): F[TurtleController[F]] =
    Ref.of(Turtle.initial)
      .map(new LocalTurtleController(id, _, eventSourcing))
}
