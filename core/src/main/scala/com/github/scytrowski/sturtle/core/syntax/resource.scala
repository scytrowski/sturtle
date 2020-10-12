package com.github.scytrowski.sturtle.core.syntax

import cats.Applicative
import cats.effect.Resource

object resource {
  implicit class ResourceFlatTap[F[_]: Applicative, A](resource: Resource[F, A]) {
    def flatTap[U](f: A => Resource[F, U]): Resource[F, A] =
      resource.flatMap { a =>
        f(a).map(_ => a)
      }
  }
}
