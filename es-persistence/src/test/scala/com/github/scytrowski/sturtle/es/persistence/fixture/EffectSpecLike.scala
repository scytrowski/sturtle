package com.github.scytrowski.sturtle.es.persistence.fixture

import java.util.concurrent.Executors

import cats.effect.concurrent.Ref
import cats.effect.{ContextShift, IO}

import scala.concurrent.ExecutionContext

trait EffectSpecLike extends CommonSpecLike {
  protected def ref[A](a: A): IO[Ref[IO, A]] = Ref.of(a)

  protected def generator[A](values: List[A], default: A): IO[IO[A]] =
    ref(values).map { r =>
      r.modify {
        case head :: tail => tail -> head
        case Nil => Nil -> default
      }
    }

  protected implicit val cs: ContextShift[IO] = IO.contextShift(ec)

  private lazy val ec: ExecutionContext = ExecutionContext.fromExecutor(Executors.newCachedThreadPool())
}
