package com.github.scytrowski.sturtle.tpl.fixture

import java.util.concurrent.Executors

import cats.effect.{ContextShift, IO}
import org.scalatest.OptionValues

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.{FiniteDuration, _}

trait EffectSpecLike extends CommonSpecLike with OptionValues {
  final protected implicit class RunTimed[A](io: IO[A]) {
    def runTimed(): A = io.unsafeRunTimed(timeout).value
  }

  protected val timeout: FiniteDuration = 5.seconds
  protected implicit val runtime: ContextShift[IO] = IO.contextShift(ExecutionContext.fromExecutor(Executors.newCachedThreadPool()))
}
