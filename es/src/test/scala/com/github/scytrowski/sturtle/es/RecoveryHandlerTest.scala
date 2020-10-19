package com.github.scytrowski.sturtle.es

import cats.effect.IO
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class RecoveryHandlerTest extends AnyWordSpec with Matchers {
  "RecoveryHandler" when {

    "recover" should {

      "remove duplicates by id" in {
        val state = recover(
          EventDescription.Event("1", 1, TestEvent("a")),
          EventDescription.Event("1", 2, TestEvent("b"))
        )

        state.data must contain oneOf("a", "b")
      }

      "sort events by timestamp" in {
        val state = recover(
          EventDescription.Event("3", 3, TestEvent("c")),
          EventDescription.Event("2", 2, TestEvent("b")),
          EventDescription.Event("1", 1, TestEvent("a"))
        )

        state.data must contain theSameElementsInOrderAs List("a", "b", "c")
      }

      "start recovery from the latest snapshot" in {
        val state = recover(
          EventDescription.Snapshot("1", 1, TestState(List("a"))),
          EventDescription.Event("2", 2, TestEvent("b")),
          EventDescription.Event("3", 3, TestEvent("c")),
          EventDescription.Snapshot("4", 4, TestState(List("d"))),
          EventDescription.Event("5", 5, TestEvent("e")),
          EventDescription.Event("6", 6, TestEvent("f"))
        )

        state.data must contain theSameElementsInOrderAs List("d", "e", "f")
      }

    }

  }

  private def recover(data: EventDescription[TestState, TestEvent]*): TestState = {
    val eventHandler = EventHandler[IO, TestState, TestEvent] { case (state, event) =>
      IO.pure(state.copy(data = state.data :+ event.newElement))
    }
    val recoveryHandler = new RecoveryHandler(TestState(List.empty), eventHandler)
    recoveryHandler.recover(data.toList).unsafeRunSync()
  }

  private case class TestState(data: List[String])

  private case class TestEvent(newElement: String)
}
