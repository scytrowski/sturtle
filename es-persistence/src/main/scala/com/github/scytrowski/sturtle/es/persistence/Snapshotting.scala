package com.github.scytrowski.sturtle.es.persistence

import cats.Applicative
import cats.effect.concurrent.Ref
import cats.effect.{Clock, Sync}
import cats.syntax.applicative._
import cats.syntax.flatMap._
import cats.syntax.functor._
import fs2.concurrent.Signal

import scala.concurrent.duration.FiniteDuration

trait Snapshotting[F[_], -S] {
  def predicate(state: S): F[Boolean]
}

object Snapshotting {
  def always[F[_]: Applicative, S]: Snapshotting[F, S] = _ => true.pure

  def never[F[_]: Applicative, S]: Snapshotting[F, S] = _ => false.pure

  def statePredicate[F[_]: Applicative, S](predicate: S => Boolean): Snapshotting[F, S] = predicate(_).pure

  def signal[F[_], S](s: Signal[F, Boolean]): Snapshotting[F, S] = _ => s.get

  def every[F[_]: Sync, S](n: Int): F[Snapshotting[F, S]] =
    Ref.of(0).map { ref =>
      val result = ref.modify { i =>
        if (i >= n) 0 -> true
        else (i + 1) -> false
      }
      _ => result
    }

  def intervals[F[_]: Sync: Clock, S](d: FiniteDuration): F[Snapshotting[F, S]] = {
    val time = Clock[F].monotonic(d.unit)
    time
      .flatMap(Ref.of(_))
      .map { ref =>
        val result = time.flatMap { current =>
          ref.modify { last =>
            if ((current - last) >= d.length) current -> true
            else last -> false
          }
        }
        _ => result
      }
  }
}
