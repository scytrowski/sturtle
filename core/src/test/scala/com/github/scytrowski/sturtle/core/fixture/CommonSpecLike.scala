package com.github.scytrowski.sturtle.core.fixture

import cats.Id
import cats.effect.{ExitCase, Sync}
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

trait CommonSpecLike extends AnyWordSpecLike with Matchers {
  protected implicit val idSync: Sync[Id] = new Sync[Id] {
    override def suspend[A](thunk: => Id[A]): Id[A] = thunk

    override def bracketCase[A, B](acquire: Id[A])(use: A => Id[B])(release: (A, ExitCase[Throwable]) => Id[Unit]): Id[B] = use(acquire)

    override def raiseError[A](e: Throwable): Id[A] = throw e

    override def handleErrorWith[A](fa: Id[A])(f: Throwable => Id[A]): Id[A] =
      try {
        fa
      } catch {
        case ex: Throwable => f(ex)
      }

    override def flatMap[A, B](fa: Id[A])(f: A => Id[B]): Id[B] = f(fa)

    override def tailRecM[A, B](a: A)(f: A => Id[Either[A, B]]): Id[B] =
      f(a) match {
        case Left(l)  => tailRecM[A, B](l)(f)
        case Right(r) => r
      }

    override def pure[A](x: A): Id[A] = x
  }
}
