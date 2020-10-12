package com.github.scytrowski.sturtle.remoting.client

import cats.effect.IO
import cats.effect.concurrent.{Deferred, Ref}
import com.github.scytrowski.sturtle.remoting.fixture.EffectSpecLike
import fs2.concurrent.Queue

class SynchronizedTurtleClientJobRegistryTest extends EffectSpecLike {
  "SynchronizedTurtleClientJobRegistry" when {

    "register" should {

      "push job to staging queue" in {
        val io =
          for {
            queue <- Queue.unbounded[IO, Deferred[IO, TurtleClientOperationResult]]
            jobs  <- Ref.of[IO, Map[String, Deferred[IO, TurtleClientOperationResult]]](Map.empty)
            jobRegistry = new SynchronizedTurtleClientJobRegistry[IO](queue, jobs)
            _ <- jobRegistry.register
            job <- queue.tryDequeue1
          } yield job
        val job = io.runTimed()

        job must be ('defined)
      }

    }

    "promote" should {

      "promote job from staging queue" in {
        val io =
          for {
            queue <- Queue.unbounded[IO, Deferred[IO, TurtleClientOperationResult]]
            jobs  <- Ref.of[IO, Map[String, Deferred[IO, TurtleClientOperationResult]]](Map.empty)
            jobRegistry = new SynchronizedTurtleClientJobRegistry[IO](queue, jobs)
            _ <- jobRegistry.register
            _ <- jobRegistry.promote("job-id")
            j <- jobs.get
          } yield j.get("job-id")
        val job = io.runTimed()

        job must be ('defined)
      }

    }

    "complete" should {

      "complete promoted job" in {
        val result = TurtleClientOperationResult.TurtleReleased
        val io =
          for {
            queue <- Queue.unbounded[IO, Deferred[IO, TurtleClientOperationResult]]
            jobs  <- Ref.of[IO, Map[String, Deferred[IO, TurtleClientOperationResult]]](Map.empty)
            jobRegistry = new SynchronizedTurtleClientJobRegistry[IO](queue, jobs)
            resultIO <- jobRegistry.register
            _ <- jobRegistry.promote("job-id")
            _ <- jobRegistry.complete("job-id", result)
            result <- resultIO
          } yield result
        val actual = io.runTimed()

        actual mustBe result
      }

    }

  }
}
