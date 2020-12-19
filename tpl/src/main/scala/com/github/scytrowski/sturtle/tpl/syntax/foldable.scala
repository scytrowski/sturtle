package com.github.scytrowski.sturtle.tpl.syntax

import cats.{Foldable, Monad}
import cats.syntax.either._
import cats.syntax.applicative._
import cats.syntax.functor._

object foldable {
  implicit class FoldLeftWhileM[F[_]: Foldable, A](fa: F[A]) {
    def foldLeftWhileM[G[_]: Monad, B](z: B)(f: (B, A) => G[Option[B]]): G[B] =
      Monad[G].tailRecM[(B, List[A]), B](z -> Foldable[F].toList(fa)) {
        case (z, head :: tail) =>
          f(z, head).map {
            case Some(z2) => Left(z2 -> tail)
            case None     => Right(z)
          }
        case (z, Nil) => z.asRight[(B, List[A])].pure
      }
  }
}
