package com.github.scytrowski.sturtle.remoting.fixture

import java.util.concurrent.Executors

import cats.effect.{ContextShift, IO}
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpecLike
import org.scalatest.{LoneElement, OptionValues}

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

trait EffectSpecLike extends AnyWordSpecLike with Matchers with LoneElement with OptionValues {
  final protected implicit class RunTimed[A](io: IO[A]) {
    def runTimed(): A = io.unsafeRunTimed(timeout).value
  }

  protected val timeout: FiniteDuration = 5.seconds
  protected implicit val cs: ContextShift[IO] = IO.contextShift(ExecutionContext.fromExecutor(Executors.newCachedThreadPool()))
}
