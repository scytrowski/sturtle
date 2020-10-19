package com.github.scytrowski.sturtle.es.persistence

import cats.effect.syntax.concurrent._
import cats.effect.{Concurrent, Resource}
import cats.syntax.apply._
import fs2.concurrent.Queue
import fs2.{Pipe, Stream}

object PushableStream {
  type Push[F[_], A] = A => F[Unit]

  def resource[F[_]: Concurrent, A, U](pipe: Pipe[F, A, U]): Resource[F, Push[F, A]] =
    for {
      queue <- Resource.liftF(Queue.noneTerminated[F, A])
      _     <- Resource.make(process(queue.dequeue, pipe).start)(queue.enqueue1(None) *> _.join)
    } yield (a: A) => queue.enqueue1(Some(a))

  private def process[F[_]: Concurrent, A, U](stream: Stream[F, A], pipe: Pipe[F, A, U]): F[Unit] =
    stream
      .through(pipe)
      .compile
      .drain
}
