package com.github.scytrowski.sturtle.core

import cats.effect.concurrent.Ref
import cats.effect.{IO, Resource}
import com.github.scytrowski.sturtle.core.TurtleCommand.MoveTo
import com.github.scytrowski.sturtle.core.TurtleQuery.GetAngle
import com.github.scytrowski.sturtle.core.fixture.EffectSpecLike
import com.github.scytrowski.sturtle.core.geometry.Point
import com.github.scytrowski.sturtle.core.mock.TestTurtleEventSourcing
import org.scalatest.LoneElement

class LocalTurtleControllerTest extends EffectSpecLike with LoneElement {
  "LocalTurtleController" when {

    "run" should {

      "handle command on event sourcing" in {
        val command = MoveTo(Point.cartesian(5, 6))
        val data = control("123")(_.run(command))

        data.commands.loneElement mustBe command
      }

      "handle query on event sourcing" in {
        val query = GetAngle
        val data = control("123")(_.execute(query))

        data.queries.loneElement mustBe query
      }

    }

  }

  private def control[U](id: String)(f: LocalTurtleController[IO] => IO[U]): TestTurtleEventSourcing.Data = {
    val resource =
      for {
        data   <- Resource.liftF(Ref.of[IO, TestTurtleEventSourcing.Data](TestTurtleEventSourcing.Data()))
        es     <- Resource.liftF(TestTurtleEventSourcing(data))
        entity <- es.entity(id)
        cntrl  = new LocalTurtleController(entity)
      } yield cntrl -> data
    val io = resource.use { case (cntrl, data) =>
      f(cntrl) *> data.get
    }
    io.unsafeRunSync()
  }

}
