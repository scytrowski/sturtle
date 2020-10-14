package com.github.scytrowski.sturtle.es.fixture

import java.util.concurrent.Executors

import cats.effect.concurrent.{MVar, MVar2, Ref}
import cats.effect.{ContextShift, IO, Timer}
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

import scala.concurrent.ExecutionContext

trait EffectSpecLike extends AnyWordSpecLike with Matchers {
  protected def mvar[A](a: A): IO[MVar2[IO, A]] =
    MVar.of(a)

  protected def ref[A](a: A): IO[Ref[IO, A]] =
    Ref.of(a)

  protected implicit val timer: Timer[IO] = IO.timer(ec)

  protected implicit val cs: ContextShift[IO] = IO.contextShift(ec)

  private lazy val ec: ExecutionContext = ExecutionContext.fromExecutor(Executors.newCachedThreadPool())
}
