package com.github.scytrowski.sturtle.core.fixture

import java.util.concurrent.Executors

import cats.effect.{ContextShift, IO}

import scala.concurrent.ExecutionContext

trait EffectSpecLike extends CommonSpecLike {
  protected implicit val cs: ContextShift[IO] = IO.contextShift(ec)

  private lazy val ec: ExecutionContext = ExecutionContext.fromExecutor(Executors.newCachedThreadPool())
}
