package com.github.scytrowski.sturtle.tpl.codegen

import com.github.scytrowski.sturtle.tpl.codegen.SyntaxTree.{Block, Expression}

sealed abstract class Case {
  def body: Block
}

object Case {
  final case class Conditional(condition: Expression, body: Block) extends Case
  final case class Default(body: Block) extends Case
}
