package com.github.scytrowski.sturtle.tpl.interpreter

trait Interpreter[F[_], C] {
  def interpret(code: C, ctx: InterpreterContext): F[InterpreterContext]
}
