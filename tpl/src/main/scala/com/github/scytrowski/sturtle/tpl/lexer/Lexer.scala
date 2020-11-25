package com.github.scytrowski.sturtle.tpl.lexer

import fs2.Pipe

import scala.language.postfixOps

trait Lexer[F[_], -S, +T] {
  def pipe: Pipe[F, S, T]
}
