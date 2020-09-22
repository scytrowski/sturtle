package com.github.scytrowski.sturtle.logging

import cats.Id
import cats.syntax.functor._
import cats.syntax.flatMap._
import cats.effect.concurrent.Ref
import com.github.scytrowski.sturtle.core.{Turtle, TurtleEventSourcing}
import com.github.scytrowski.sturtle.core.TurtleCommand.{MoveTo, RotateTo}
import com.github.scytrowski.sturtle.core.TurtleEvent.RotatedTo
import com.github.scytrowski.sturtle.core.TurtleQuery.GetFillColor
import com.github.scytrowski.sturtle.geometry.{Angle, Point}
import com.github.scytrowski.sturtle.logging.fixture.CommonSpecLike
import com.github.scytrowski.sturtle.logging.mock.{TestTurtleEventSourcingDescription, TestTurtleLogger}

class LoggingExtensionTest extends CommonSpecLike {
  "LoggingExtension" should {

    "log command" in {
      val command = MoveTo(Point.cartesian(5, 10))

      val data = test { es =>
        es.run(Turtle.initial, command)
      }

      data.commands mustBe List(command)
    }

    "log events" in {
      val angle = Angle.radians(1.23)
      val command = RotateTo(angle)

      val data = test { es =>
        es.run(Turtle.initial, command)
      }

      data.events mustBe List(RotatedTo(angle))
    }

    "log query" in {
      val query = GetFillColor

      val data = test { es =>
        es.execute(Turtle.initial, query)
      }

      data.queries mustBe List(query)
    }

  }

  private def test[U](f: TurtleEventSourcing[Id] => Id[U]): TestTurtleLogger.TestData =
    for {
      data   <- Ref.of(TestTurtleLogger.TestData())
      logger = new TestTurtleLogger(data)
      ext    = new LoggingExtension[Id](logger)
      _      <- ext.eventSourcing(TestTurtleEventSourcingDescription[Id]).use(f)
      d      <- data.get
    } yield d
}
