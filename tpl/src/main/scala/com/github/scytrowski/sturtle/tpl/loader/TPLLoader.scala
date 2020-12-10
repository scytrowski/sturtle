package com.github.scytrowski.sturtle.tpl.loader

import cats.Applicative
import cats.syntax.applicative._
import cats.instances.set._
import com.github.scytrowski.sturtle.tpl.interpreter._
import com.github.scytrowski.sturtle.tpl.module.Module

final class TPLLoader[F[_]: Applicative](interpreter: Interpreter[F, TPLCode]) extends Loader[F] {
  override def load(module: Module[F]): F[InterpreterContext[F]] = {
    module match {
      case Module.Raw(code) => interpreter.interpret(code, InterpreterContext.initial)
      case prepared: Module.Prepared[F] =>
        InterpreterContext.initial[F].putObjects(prepared.objects).pure
    }
  }
}
