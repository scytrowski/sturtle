package com.github.scytrowski.sturtle.tpl.mock

import cats.effect.concurrent.Ref
import cats.effect.{IO, Resource}
import com.github.scytrowski.sturtle.core.{TurtleCommand, TurtleController, TurtleQuery, TurtleQueryAnswer, TurtleRef, TurtleRefProvider}
import com.github.scytrowski.sturtle.tpl.mock.TestTurtleRefProvider.TestData

 class TestTurtleRefProvider(data: Ref[IO, TestData]) extends TurtleRefProvider[IO] {
  override def ref(id: String): TurtleRef[IO] = new TestTurtleRef(id, data)
}

object TestTurtleRefProvider {
  final case class TestData(commands: List[TurtleCommand],
                            queries: List[TurtleQuery],
                            answer: TurtleQueryAnswer)
}

private final class TestTurtleRef(override val id: String, data: Ref[IO, TestData]) extends TurtleRef[IO] {
  override def controller: Resource[IO, TurtleController[IO]] = Resource.pure[IO, TestTurtleController](new TestTurtleController(data))
}

private final class TestTurtleController(data: Ref[IO, TestData]) extends TurtleController[IO] {
  override def run(command: TurtleCommand): IO[Unit] = data.update(d => d.copy(commands = d.commands :+ command))

  override def execute(query: TurtleQuery): IO[TurtleQueryAnswer] =
    data
      .modify { d =>
        d.copy(queries = d.queries :+ query) -> d.answer
      }
}
