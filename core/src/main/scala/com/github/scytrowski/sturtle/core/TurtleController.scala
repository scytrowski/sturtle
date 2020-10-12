package com.github.scytrowski.sturtle.core

import cats.Monad
import cats.syntax.functor._
import cats.syntax.flatMap._
import cats.effect.Sync
import cats.effect.concurrent.Ref

trait TurtleController[F[_]] {
  def run(command: TurtleCommand): F[Unit]

  def execute(query: TurtleQuery): F[TurtleQueryAnswer]
}

private[core] final class LocalTurtleController[F[_]: Monad] private(val id: String,
                                                       turtleRef: Ref[F, Turtle],
                                                       eventSourcing: TurtleEventSourcing[F]) extends TurtleController[F] {
  override def run(command: TurtleCommand): F[Unit] =
    for {
      turtle        <- turtleRef.get
      updatedTurtle <- eventSourcing.run(turtle, command)
      _             <- turtleRef.set(updatedTurtle)
    } yield ()

  override def execute(query: TurtleQuery): F[query.Answer] =
    for {
      turtle <- turtleRef.get
      answer <- eventSourcing.execute(turtle, query)
    } yield answer
}

private[core] object LocalTurtleController {
  def apply[F[_]: Sync](id: String, eventSourcing: TurtleEventSourcing[F]): F[TurtleController[F]] =
    Ref.of(Turtle.initial)
      .map(new LocalTurtleController(id, _, eventSourcing))
}