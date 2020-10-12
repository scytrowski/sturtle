package com.github.scytrowski.sturtle.remoting.mock

import cats.effect.concurrent.Ref
import cats.effect.{IO, Resource}
import com.github.scytrowski.sturtle.core.{TurtleCommand, TurtleController, TurtleQuery, TurtleQueryAnswer, TurtleRef}
import com.github.scytrowski.sturtle.remoting.mock.TestTurtleRef.Data

final class TestTurtleRef(val id: String,
                          data: Ref[IO, Data],
                          answer: TurtleQueryAnswer) extends TurtleRef[IO] {
  override def controller: Resource[IO, TurtleController[IO]] =
    Resource.pure[IO, TurtleController[IO]](new TestTurtleController(data, answer))
}

object TestTurtleRef {
  final case class Data(commands: List[TurtleCommand] = Nil,
                        queries: List[TurtleQuery] = Nil)
}

private final class TestTurtleController(data: Ref[IO, Data],
                                         answer: TurtleQueryAnswer) extends TurtleController[IO] {
  override def run(command: TurtleCommand): IO[Unit] =
    data.update(d => d.copy(commands = d.commands :+ command))

  override def execute(query: TurtleQuery): IO[TurtleQueryAnswer] =
    data.update(d => d.copy(queries = d.queries :+ query)).as(answer)
}
