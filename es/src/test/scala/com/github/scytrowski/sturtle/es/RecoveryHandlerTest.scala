package com.github.scytrowski.sturtle.es

import cats.effect.IO
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class RecoveryHandlerTest extends AnyWordSpec with Matchers {
  "RecoveryHandler" when {

    "recover" should {

      "remove duplicates by id" in {
        val state = recover(
          RecoveryData.Event("1", 1, TestEvent("a")),
          RecoveryData.Event("1", 2, TestEvent("b"))
        )

        state.data must contain oneOf("a", "b")
      }

      "sort events by timestamp" in {
        val state = recover(
          RecoveryData.Event("3", 3, TestEvent("c")),
          RecoveryData.Event("2", 2, TestEvent("b")),
          RecoveryData.Event("1", 1, TestEvent("a"))
        )

        state.data must contain theSameElementsInOrderAs List("a", "b", "c")
      }

      "start recovery from the latest snapshot" in {
        val state = recover(
          RecoveryData.Snapshot("1", 1, TestState(List("a"))),
          RecoveryData.Event("2", 2, TestEvent("b")),
          RecoveryData.Event("3", 3, TestEvent("c")),
          RecoveryData.Snapshot("4", 4, TestState(List("d"))),
          RecoveryData.Event("5", 5, TestEvent("e")),
          RecoveryData.Event("6", 6, TestEvent("f"))
        )

        state.data must contain theSameElementsInOrderAs List("d", "e", "f")
      }

    }

  }

  private def recover(data: RecoveryData[TestState, TestEvent]*): TestState = {
    val eventHandler = EventHandler[IO, TestState, TestEvent] { case (state, event) =>
      IO.pure(state.copy(data = state.data :+ event.newElement))
    }
    val recoveryHandler = new RecoveryHandler(TestState(List.empty), eventHandler)
    recoveryHandler.recover(data.toList).unsafeRunSync()
  }

  private case class TestState(data: List[String])

  private case class TestEvent(newElement: String)
}
