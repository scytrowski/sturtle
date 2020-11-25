package com.github.scytrowski.sturtle.tpl.interpreter

import cats.data.NonEmptyList
import com.github.scytrowski.sturtle.tpl.interpreter.SyntaxTree.Expression.Name

sealed abstract class SyntaxTree

object SyntaxTree {
  final case class Block(statements: List[SyntaxTree]) extends SyntaxTree

  sealed abstract class Expression extends SyntaxTree

  object Expression {
    final case class Name(value: String) extends Expression
    final case class Static(value: Value) extends Expression
    final case class FunctionCall(name: Name, parameters: List[Expression]) extends Expression
  }

  final case class FunctionDefinition(name: Name, parameters: List[Name], body: Block) extends SyntaxTree
  final case class Branch(cases: NonEmptyList[Case]) extends SyntaxTree
  final case class Loop(condition: Expression, body: Block) extends SyntaxTree
  final case class Assignment(variableName: Name, value: Expression) extends SyntaxTree
  final case class Return(expression: Expression) extends SyntaxTree
  case object Break extends SyntaxTree
}
