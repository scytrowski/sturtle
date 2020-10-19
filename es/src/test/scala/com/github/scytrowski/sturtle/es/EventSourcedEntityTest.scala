package com.github.scytrowski.sturtle.es

import cats.effect.IO
import com.github.scytrowski.sturtle.es.fixture.EffectSpecLike
import com.github.scytrowski.sturtle.es.mock.TestEventSourcingDescription.{TestCommand, TestEvent, TestQuery, TestState}
import com.github.scytrowski.sturtle.es.mock.{TestEventSourcing, TestEventSourcingDescription, TestEventSourcingSinks, TestEventStore, TestEventStoreSession}
import org.scalatest.LoneElement

class EventSourcedEntityTest extends EffectSpecLike with LoneElement {
  "EventSourcedEntity" when {

    "run" should {

      "run command using event sourcing description" in {
        val command = TestCommand("a")
        val (_, sinkData, storeData) = useEntity()(_.run(command))

        sinkData.commands.loneElement mustBe command
        sinkData.events.loneElement mustBe TestEvent(command.data)
        storeData.persist.loneElement mustBe TestEvent(command.data)
        storeData.snapshotCandidate.loneElement mustBe TestState(List("a"))
      }

      "run sequence of commands using event sourcing description" in {
        val firstCommand = TestCommand("a")
        val secondCommand = TestCommand("b")
        val (_, sinkData, storeData) = useEntity()(e => e.run(firstCommand) *> e.run(secondCommand))

        sinkData.commands must contain theSameElementsInOrderAs List(firstCommand, secondCommand)
        sinkData.events must contain theSameElementsInOrderAs List(TestEvent("a"), TestEvent("b"))
        storeData.persist must contain theSameElementsInOrderAs List(TestEvent("a"), TestEvent("b"))
        storeData.snapshotCandidate must contain theSameElementsInOrderAs List(TestState(List("a")), TestState(List("a", "b")))
      }

    }

    "execute" should {

      "execute query using event sourcing description" in {
        val elements = List("a", "b", "c")
        val query = TestQuery
        val (answer, sinkData, _) = useEntity(TestState(elements))(_.execute(query))

        answer mustBe elements
        sinkData.queries.loneElement mustBe query
      }

    }

  }

  private def useEntity[U](initialState: TestState = TestState(Nil))(f: TestEventSourcing.Entity => IO[U]): (U, TestEventSourcing.Data, TestEventStore.Data) = {
    val io =
      for {
        sinkDataRef <- ref(TestEventSourcing.Data())
        storeDataRef <- ref(TestEventStore.Data())
        state <- mvar(initialState)
        entity = new EventSourcedEntity(
          TestEventSourcingDescription(),
          new TestEventSourcingSinks(sinkDataRef),
          new TestEventStoreSession(storeDataRef),
          state
        )
        result <- f(entity)
        sinkData <- sinkDataRef.get
        storeData <- storeDataRef.get
      } yield (result, sinkData, storeData)
    io.unsafeRunSync()
  }
}
