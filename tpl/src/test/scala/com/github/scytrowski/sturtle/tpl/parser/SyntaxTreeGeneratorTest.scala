package com.github.scytrowski.sturtle.tpl.parser

import com.github.scytrowski.sturtle.tpl.codegen.SyntaxTree.{Assignment, Block, Break, Loop, Return}
import com.github.scytrowski.sturtle.tpl.codegen.SyntaxTree.Expression.{FunctionCall, Name, Static}
import com.github.scytrowski.sturtle.tpl.fixture.CommonSpecLike
import com.github.scytrowski.sturtle.tpl.interpreter.Value.NumberValue

class SyntaxTreeGeneratorTest extends CommonSpecLike with SyntaxTreeGenerator { gen: SyntaxTreeGenerator =>
  "SyntaxTreeGenerator" should {
    "generate repeat" in {
      val counter = Name("i")
      val times = Name("n")
      val body = Block(List(Break))

      gen.repeat(times, body, counter) mustBe Block(List(
        Assignment(counter, Static(NumberValue(0))),
        Loop(
          FunctionCall(SpecialNames.lessOrEqual, List(counter, times)),
          Block(
            FunctionCall(SpecialNames.add, List(counter, Static(NumberValue(1)))) +:
              body.statements
          )
        )
      ))
    }

    "generate loop" in {
      val condition = Name("p")
      val body = Block(List(Break))

      gen.loop(condition, body) mustBe Loop(condition, body)
    }

    "generate block" in {
      val st1 = Break
      val st2 = Return(Name("r"))

      gen.block(st1, st2) mustBe Block(List(st1, st2))
    }

    "generate assignment" in {
      val name = Name("a")
      val value = Name("b")

      gen.assignment(name, value) mustBe Assignment(name, value)
    }

    "generate equal" in {
      val a = Name("a")
      val b = Name("b")

      gen.equal(a, b) mustBe FunctionCall(SpecialNames.equal, List(a, b))
    }

    "generate not equal" in {
      val a = Name("a")
      val b = Name("b")

      gen.notEqual(a, b) mustBe FunctionCall(SpecialNames.notEqual, List(a, b))
    }

    "generate less" in {
      val a = Name("a")
      val b = Name("b")

      gen.less(a, b) mustBe FunctionCall(SpecialNames.less, List(a, b))
    }

    "generate lessOrEqual" in {
      val a = Name("a")
      val b = Name("b")

      gen.lessOrEqual(a, b) mustBe FunctionCall(SpecialNames.lessOrEqual, List(a, b))
    }

    "generate greater" in {
      val a = Name("a")
      val b = Name("b")

      gen.greater(a, b) mustBe FunctionCall(SpecialNames.greater, List(a, b))
    }

    "generate greaterOrEqual" in {
      val a = Name("a")
      val b = Name("b")

      gen.greaterOrEqual(a, b) mustBe FunctionCall(SpecialNames.greaterOrEqual, List(a, b))
    }

    "generate negate" in {
      val a = Name("a")

      gen.negate(a) mustBe FunctionCall(SpecialNames.negate, List(a))
    }

    "generate and" in {
      val a = Name("a")
      val b = Name("b")

      gen.and(a, b) mustBe FunctionCall(SpecialNames.and, List(a, b))
    }

    "generate or" in {
      val a = Name("a")
      val b = Name("b")

      gen.or(a, b) mustBe FunctionCall(SpecialNames.or, List(a, b))
    }

    "generate plus" in {
      val a = Name("a")

      gen.plus(a) mustBe FunctionCall(SpecialNames.plus, List(a))
    }

    "generate increment" in {
      val a = Name("a")

      gen.increment(a) mustBe FunctionCall(SpecialNames.add, List(a, Static(NumberValue(1))))
    }

    "generate add" in {
      val a = Name("a")
      val b = Name("b")

      gen.add(a, b) mustBe FunctionCall(SpecialNames.add, List(a, b))
    }

    "generate minus" in {
      val a = Name("a")

      gen.minus(a) mustBe FunctionCall(SpecialNames.minus, List(a))
    }

    "generate subtract" in {
      val a = Name("a")
      val b = Name("b")

      gen.subtract(a, b) mustBe FunctionCall(SpecialNames.sub, List(a, b))
    }

    "generate multi" in {
      val a = Name("a")
      val b = Name("b")

      gen.multi(a, b) mustBe FunctionCall(SpecialNames.multi, List(a, b))
    }

    "generate div" in {
      val a = Name("a")
      val b = Name("b")

      gen.div(a, b) mustBe FunctionCall(SpecialNames.div, List(a, b))
    }

    "generate number" in {
      val v = -1.337

      gen.number(v) mustBe Static(NumberValue(v))
    }

    "generate point" in {
      val a = Name("a")
      val b = Name("b")

      gen.point(a, b) mustBe FunctionCall(SpecialNames.point, List(a, b))
    }

    "generate vector" in {
      val a = Name("a")
      val b = Name("b")

      gen.vector(a, b) mustBe FunctionCall(SpecialNames.vector, List(a, b))
    }

    "generate angle" in {
      val a = Name("a")

      gen.angle(a) mustBe FunctionCall(SpecialNames.angle, List(a))
    }

    "generate color" in {
      val a = Name("a")
      val b = Name("b")
      val c = Name("c")

      gen.color(a, b, c) mustBe FunctionCall(SpecialNames.color, List(a, b, c))
    }
  }
}
