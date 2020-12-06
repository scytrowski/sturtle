package com.github.scytrowski.sturtle.tpl.parser

import com.github.scytrowski.sturtle.tpl.codegen.SyntaxTree.Expression
import com.github.scytrowski.sturtle.tpl.parser.ParseError.UnexpectedEndOfStream

sealed abstract class Operator(val precedence: Int) {
  def parse: Parse[Expression, Expression]
}

object Operator extends SyntaxTreeGenerator { gen: SyntaxTreeGenerator =>
  sealed abstract class UnaryOperator(precedence: Int, expF: Expression => Expression) extends Operator(precedence) {
    override def parse: Parse[Expression, Expression] = {
      case head :: tail => ParseResult.Success(expF(head), tail)
      case _ => ParseResult.Failure(UnexpectedEndOfStream)
    }
  }

  sealed abstract class BinaryOperator(precedence: Int, expF: (Expression, Expression) => Expression) extends Operator(precedence) {
    final override def parse: Parse[Expression, Expression] = {
      case left :: right :: tail => ParseResult.Success(expF(right, left), tail)
      case _ => ParseResult.Failure(UnexpectedEndOfStream)
    }
  }

  case object And extends BinaryOperator(1, gen.and)
  case object Or extends BinaryOperator(1, gen.or)
  case object Negate extends UnaryOperator(2, gen.negate)
  case object Equal extends BinaryOperator(3, gen.equal)
  case object NotEqual extends BinaryOperator(3, gen.notEqual)
  case object Less extends BinaryOperator(3, gen.less)
  case object LessOrEqual extends BinaryOperator(3, gen.lessOrEqual)
  case object Greater extends BinaryOperator(3, gen.greater)
  case object GreaterOrEqual extends BinaryOperator(3, gen.greaterOrEqual)
  case object Add extends BinaryOperator(4, gen.add)
  case object Subtract extends BinaryOperator(4, gen.subtract)
  case object Multiply extends BinaryOperator(5, gen.multi)
  case object Divide extends BinaryOperator(5, gen.div)
  case object Plus extends UnaryOperator(6, gen.plus)
  case object Minus extends UnaryOperator(6, gen.minus)
}
