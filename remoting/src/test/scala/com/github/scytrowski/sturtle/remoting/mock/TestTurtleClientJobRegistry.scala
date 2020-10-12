package com.github.scytrowski.sturtle.remoting.mock

import cats.effect.IO
import cats.effect.concurrent.Ref
import com.github.scytrowski.sturtle.remoting.client.{TurtleClientJobRegistry, TurtleClientOperationResult}

class TestTurtleClientJobRegistry(data: Ref[IO, TestTurtleClientJobRegistry.Data]) extends TurtleClientJobRegistry[IO] {
  override def register: IO[IO[TurtleClientOperationResult]] =
    data.modify(d =>
      d.results match {
        case head :: tail => d.copy(register = d.register + 1, results = tail) -> head
        case Nil => d.copy(register = d.register + 1) -> TurtleClientOperationResult.NoTurtleSelected
      }
    ).map(IO.pure)

  override def promote(id: String): IO[Unit] =
    data.update(d => d.copy(promote = d.promote :+ id))

  override def complete(id: String, result: TurtleClientOperationResult): IO[Unit] =
    data.update(d => d.copy(complete =  d.complete :+ (id -> result)))
}

object TestTurtleClientJobRegistry {
  final case class Data(register: Int = 0,
                        promote: List[String] = Nil,
                        complete: List[(String, TurtleClientOperationResult)] = Nil,
                        results: List[TurtleClientOperationResult] = Nil)
}
