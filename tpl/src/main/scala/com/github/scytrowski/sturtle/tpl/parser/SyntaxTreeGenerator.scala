package com.github.scytrowski.sturtle.tpl.parser

import com.github.scytrowski.sturtle.tpl.codegen.SyntaxTree.Expression.{FunctionCall, Name, Static}
import com.github.scytrowski.sturtle.tpl.codegen.SyntaxTree.{Assignment, Block, Expression, Loop}
import com.github.scytrowski.sturtle.tpl.interpreter.NumberValue

trait SyntaxTreeGenerator {
  protected def repeat(times: Expression, body: Block, counter: Name = SpecialNames.temporaryVariable): Block =
    Block(List(
      assignment(counter, number(0)),
      loop(
        lessOrEqual(counter, times),
        Block(increment(counter) +: body.statements)
      )
    ))

  protected def loop(condition: Expression, body: Block): Loop = Loop(condition, body)

  protected def assignment(name: Name, value: Expression): Assignment =
    Assignment(name, value)

  protected def equal(left: Expression, right: Expression): Expression =
    FunctionCall(SpecialNames.equal, List(left, right))

  protected def notEqual(left: Expression, right: Expression): Expression =
    FunctionCall(SpecialNames.notEqual, List(left, right))

  protected def less(left: Expression, right: Expression): Expression =
    FunctionCall(SpecialNames.less, List(left, right))

  protected def lessOrEqual(left: Expression, right: Expression): Expression =
    FunctionCall(SpecialNames.lessOrEqual, List(left, right))

  protected def greater(left: Expression, right: Expression): Expression =
    FunctionCall(SpecialNames.greater, List(left, right))

  protected def greaterOrEqual(left: Expression, right: Expression): Expression =
    FunctionCall(SpecialNames.greaterOrEqual, List(left, right))

  protected def negate(value: Expression): Expression =
    FunctionCall(SpecialNames.negate, List(value))

  protected def and(left: Expression, right: Expression): Expression =
    FunctionCall(SpecialNames.and, List(left, right))

  protected def or(left: Expression, right: Expression): Expression =
    FunctionCall(SpecialNames.or, List(left, right))

  protected def plus(value: Expression): Expression =
    FunctionCall(SpecialNames.plus, List(value))

  protected def increment(expr: Expression): Expression =
    add(expr, number(1))

  protected def add(left: Expression, right: Expression): Expression =
    FunctionCall(SpecialNames.add, List(left, right))

  protected def minus(value: Expression): Expression =
    FunctionCall(SpecialNames.minus, List(value))

  protected def subtract(left: Expression, right: Expression): Expression =
    FunctionCall(SpecialNames.sub, List(left, right))

  protected def multi(left: Expression, right: Expression): Expression =
    FunctionCall(SpecialNames.multi, List(left, right))

  protected def div(left: Expression, right: Expression): Expression =
    FunctionCall(SpecialNames.div, List(left, right))

  protected def number(value: Double): Expression = Static(NumberValue(value))

  protected def point(first: Expression, second: Expression): Expression =
    FunctionCall(SpecialNames.point, List(first, second))

  protected def vector(first: Expression, second: Expression): Expression =
    FunctionCall(SpecialNames.vector, List(first, second))

  protected def angle(value: Expression): Expression =
    FunctionCall(SpecialNames.angle, List(value))

  protected def color(first: Expression, second: Expression, third: Expression): Expression =
    FunctionCall(SpecialNames.color, List(first, second, third))
}
