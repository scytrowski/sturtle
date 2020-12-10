package com.github.scytrowski.sturtle.tpl.loader

import cats.{Monad, Traverse}
import cats.syntax.functor._
import cats.syntax.foldable._
import cats.syntax.traverse._
import com.github.scytrowski.sturtle.tpl.interpreter.InterpreterContext
import com.github.scytrowski.sturtle.tpl.module.Module

trait Loader[F[_]] {
  final def loadAll[G[_]: Traverse](modules: G[Module[F]])
                                   (implicit monad: Monad[F]): F[InterpreterContext[F]] =
    modules
      .traverse(load)
      .map(_.foldLeft(InterpreterContext.initial[F])(_ <+> _))

  def load(module: Module[F]): F[InterpreterContext[F]]
}
