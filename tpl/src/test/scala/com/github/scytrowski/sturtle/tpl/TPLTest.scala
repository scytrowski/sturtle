package com.github.scytrowski.sturtle.tpl

import cats.effect.IO
import cats.effect.concurrent.Ref
import com.github.scytrowski.sturtle.core.TurtleQueryAnswer
import com.github.scytrowski.sturtle.core.TurtleQueryAnswer.PositionAnswer
import com.github.scytrowski.sturtle.core.geometry.Point
import com.github.scytrowski.sturtle.tpl.fixture.{EffectSpecLike, RandomnessFixture, TableFixture}
import com.github.scytrowski.sturtle.tpl.interpreter.{InterpreterContext, NumberValue}
import com.github.scytrowski.sturtle.tpl.mock.TestTurtleRefProvider
import com.github.scytrowski.sturtle.tpl.mock.TestTurtleRefProvider.TestData
import com.github.scytrowski.sturtle.tpl.types.Complex
import fs2.Stream

class TPLTest extends EffectSpecLike with RandomnessFixture with TableFixture {
  "TPL" should {
    "define simple math function" in {
      forAll(Table("z", randomElements[Complex](100):_*)) { z =>
        val source =
          s"""
            |function f(x)
            |   return x * 2
            |end
            |
            |f($z)
            |""".stripMargin

        val (ctx, _) = run(source)

        ctx.stack.asLazyList.toList mustBe List(NumberValue(z * 2))
      }
    }

    "compute minimum of 3 numbers" in {
      forAll(Table(("a", "b", "c"), randomTriples[Double](50):_*)) { case (a, b, c) =>
        val source =
          s"""
             |function min3(a, b, c)
             |   return min2(min2(a, b), c)
             |end
             |
             |function min2(a, b)
             |   if a <= b then
             |      return a
             |   else
             |      return b
             |   end
             |end
             |
             |min3($a, $b, $c)
             |""".stripMargin

        val (ctx, _) = run(source)
        val expectedMin = List(a, b, c).min

        ctx.stack.asLazyList.toList mustBe List(NumberValue(Complex.real(expectedMin)))
      }
    }

    "compute maximum of 3 numbers" in {
      forAll(Table(("a", "b", "c"), randomTriples[Double](50):_*)) { case (a, b, c) =>
        val source =
          s"""
             |function max3(a, b, c)
             |   return max2(max2(a, b), c)
             |end
             |
             |function max2(a, b)
             |   if a >= b then
             |      return a
             |   else
             |      return b
             |   end
             |end
             |
             |max3($a, $b, $c)
             |""".stripMargin

        val (ctx, _) = run(source)
        val expectedMax = List(a, b, c).max

        ctx.stack.asLazyList.toList mustBe List(NumberValue(Complex.real(expectedMax)))
      }
    }

    "compute Fibonacci numbers" when {
      lazy val lazyFibs: LazyList[Int] = 0 #:: 1 #:: lazyFibs.zip(lazyFibs.tail).map { case (a, b) => a + b }
      val checksCount = 20

      "iteratively" in {
        val fibs = lazyFibs.take(checksCount)

        forAll(Table("n", 0.until(checksCount):_*)) { n =>
          val source =
            s"""
               |function fib(n)
               |   a := 0
               |   b := 1
               |   i := 0
               |   while i < n do
               |      t := b
               |      b := a + b
               |      a := t
               |      i := i + 1
               |   end
               |   return a
               |end
               |
               |fib($n)
               |""".stripMargin

          val (ctx, _) = run(source)

          ctx.stack.asLazyList.toList mustBe List(NumberValue(Complex.real(fibs(n))))
        }
      }

      "recursively" in {
        val fibs = lazyFibs.take(checksCount)

        forAll(Table("n", 0.until(checksCount):_*)) { n =>
          val source =
            s"""
               |function fib(n)
               |   if n = 0 then
               |      return 0
               |   elif n = 1 then
               |      return 1
               |   else
               |      return fib(n - 1) + fib(n - 2)
               |   end
               |end
               |
               |fib($n)
               |""".stripMargin

          val (ctx, _) = run(source)

          ctx.stack.asLazyList.toList mustBe List(NumberValue(Complex.real(fibs(n))))
        }
      }
    }

    "compute factorials" when {
      lazy val lazyFacts: LazyList[Int] = LazyList.iterate(2)(_ + 1).scanLeft(1)(_ * _)
      val checksCount = 10

      "iteratively" in {
        val facts = lazyFacts.take(checksCount)

        forAll(Table("n", 0.until(checksCount):_*)) { n =>
          val source =
            s"""
               |function fact(n)
               |   r := 1
               |   i := 2
               |   while i <= n do
               |      r := r * i
               |      i := i + 1
               |   end
               |   return r
               |end
               |
               |fact(${n + 1})
               |""".stripMargin

          val (ctx, _) = run(source)

          ctx.stack.asLazyList.toList mustBe List(NumberValue(Complex.real(facts(n))))
        }
      }

      "recursively" in {
        val facts = lazyFacts.take(checksCount)

        forAll(Table("n", 0.until(checksCount):_*)) { n =>
          val source =
            s"""
               |function fact(n)
               |   if n = 1 then
               |      return 1
               |   else
               |      return n * fact(n - 1)
               |   end
               |end
               |
               |fact(${n + 1})
               |""".stripMargin

          val (ctx, _) = run(source)

          ctx.stack.asLazyList.toList mustBe List(NumberValue(Complex.real(facts(n))))
        }
      }
    }
  }

  private def run(source: String,
                  answer: TurtleQueryAnswer = PositionAnswer(Point(1, 2))): (InterpreterContext[IO], TestData) = {
    val io =
      for {
        dataRef <- Ref.of[IO, TestData](TestData(Nil, Nil, answer))
        ctx     <- new TPL[IO].run(TPLConfig(new TestTurtleRefProvider(dataRef), Nil), "test-id", Stream(source))
        data    <- dataRef.get
      } yield ctx -> data
    io.unsafeRunSync()
  }
}
