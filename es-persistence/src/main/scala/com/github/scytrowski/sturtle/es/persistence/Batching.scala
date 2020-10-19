package com.github.scytrowski.sturtle.es.persistence

import cats.effect.{Concurrent, Timer}
import fs2.{Chunk, Pipe}

import scala.concurrent.duration.FiniteDuration

trait Batching[F[_]] {
  def pipe[A]: Pipe[F, A, Chunk[A]]
}

object Batching {
  def fixedSize[F[_]](size: Int): Batching[F] =
    new Batching[F] {
      override def pipe[A]: Pipe[F, A, Chunk[A]] = _.chunkN(size)
    }

  def timeWindows[F[_]: Concurrent: Timer](interval: FiniteDuration, maxSize: Int): Batching[F] =
    new Batching[F] {
      override def pipe[A]: Pipe[F, A, Chunk[A]] = _.groupWithin(maxSize, interval)
    }
}


