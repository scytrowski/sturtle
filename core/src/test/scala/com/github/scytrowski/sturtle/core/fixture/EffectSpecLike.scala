package com.github.scytrowski.sturtle.core.fixture

import java.util.concurrent.Executors

import cats.effect.{ContextShift, IO}
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

import scala.concurrent.ExecutionContext

trait EffectSpecLike extends AnyWordSpecLike with Matchers {
  protected implicit val cs: ContextShift[IO] = IO.contextShift(ec)

  private lazy val ec: ExecutionContext = ExecutionContext.fromExecutor(Executors.newCachedThreadPool())
}
