package com.github.scytrowski.sturtle.tpl.interpreter

import com.github.scytrowski.sturtle.tpl.interpreter.SyntaxTree.{Block, Expression}

sealed abstract class Case {
  def body: Block
}

object Case {
  final case class ConditionalCase(condition: Expression, body: Block) extends Case
  final case class DefaultCase(body: Block) extends Case
}
