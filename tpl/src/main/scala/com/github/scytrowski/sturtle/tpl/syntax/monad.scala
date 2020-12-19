package com.github.scytrowski.sturtle.tpl.syntax

import cats.{Foldable, Monad}
import cats.syntax.functor._
import cats.syntax.flatMap._

object monad {
  implicit class FindFirst[F[_]](monad: Monad[F]) {
    def findFirst[G[_]: Foldable, A](g: G[A])(cond: A => F[Boolean]): F[Option[A]] =
      Foldable[G].collectFirstSomeM(g) { a =>
        monad.map(cond(a))(r => Some(a).filter(_ => r))
      }(monad)
  }

  implicit class FoldWhileM[F[_]: Monad, A](fa: F[A]) {
    def foldWhileM[B](f: A => F[Option[A]]): F[A] =
      fa.flatMap {
        Monad[F].tailRecM[A, A](_) { a =>
          f(a).map {
            case Some(a2) => Left(a2)
            case None     => Right(a)
          }
        }
      }

    def collect[B](pf: PartialFunction[A, B]): F[Option[B]] = fa.map(pf.lift)
  }
}
