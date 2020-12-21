package com.github.scytrowski.sturtle.tpl.module

import cats.effect.IO
import cats.syntax.either._
import com.github.scytrowski.sturtle.core.geometry.{Angle, Point, Vector}
import com.github.scytrowski.sturtle.core.graphics.Color
import com.github.scytrowski.sturtle.tpl.fixture.{EffectSpecLike, RandomnessFixture, TableFixture}
import com.github.scytrowski.sturtle.tpl.interpreter.InterpreterError.{DivisionByZero, IllegalParameter}
import com.github.scytrowski.sturtle.tpl.interpreter._
import com.github.scytrowski.sturtle.tpl.types.Nat._0
import com.github.scytrowski.sturtle.tpl.types.{Complex, Nat}
import org.scalatest.Inside

class SpecialFunctionsModuleTest extends EffectSpecLike with RandomnessFixture with TableFixture with Inside {
  "SpecialFunctionsModule" when {
    "equal" should {
      "return true" in {
        val value = StringValue("testtest123456")

        invokeFunction(SpecialFunctions.equal, value, value) mustBe BooleanValue(true)
      }

      "return false" in {
        val left = StringValue("1337")
        val right = NumberValue(Complex.real(1337))

        invokeFunction(SpecialFunctions.equal, left, right) mustBe BooleanValue(false)
      }
    }

    "notEqual" should {
      "return true" in {
        val value = StringValue("testtest123456")

        invokeFunction(SpecialFunctions.notEqual, value, value) mustBe BooleanValue(false)
      }

      "return false" in {
        val left = StringValue("1337")
        val right = NumberValue(Complex.real(1337))

        invokeFunction(SpecialFunctions.notEqual, left, right) mustBe BooleanValue(true)
      }
    }

    "less" in {
      forAll(Table(("l", "r"), randomPairs[Double](100):_*)) { case (l, r) =>
        val left = NumberValue(Complex.real(l))
        val right = NumberValue(Complex.real(r))

        invokeFunction(SpecialFunctions.less, left, right) mustBe BooleanValue(l < r)
      }
    }

    "lessOrEqual" in {
      forAll(Table(("l", "r"), randomPairs[Double](100):_*)) { case (l, r) =>
        val left = NumberValue(Complex.real(l))
        val right = NumberValue(Complex.real(r))

        invokeFunction(SpecialFunctions.lessOrEqual, left, right) mustBe BooleanValue(l <= r)
      }
    }

    "greater" in {
      forAll(Table(("l", "r"), randomPairs[Double](100):_*)) { case (l, r) =>
        val left = NumberValue(Complex.real(l))
        val right = NumberValue(Complex.real(r))

        invokeFunction(SpecialFunctions.greater, left, right) mustBe BooleanValue(l > r)
      }
    }

    "greaterOrEqual" in {
      forAll(Table(("l", "r"), randomPairs[Double](100):_*)) { case (l, r) =>
        val left = NumberValue(Complex.real(l))
        val right = NumberValue(Complex.real(r))

        invokeFunction(SpecialFunctions.greaterOrEqual, left, right) mustBe BooleanValue(l >= r)
      }
    }

    "negate" in {
      forAll(truthTable1) { p =>
        val value = BooleanValue(p)

        invokeFunction(SpecialFunctions.negate, value) mustBe BooleanValue(!p)
      }
    }

    "and" in {
      forAll(truthTable2) { case (p, q) =>
        val left = BooleanValue(p)
        val right = BooleanValue(q)

        invokeFunction(SpecialFunctions.and, left, right) mustBe BooleanValue(p && q)
      }
    }

    "or" in {
      forAll(truthTable2) { case (p, q) =>
        val left = BooleanValue(p)
        val right = BooleanValue(q)

        invokeFunction(SpecialFunctions.or, left, right) mustBe BooleanValue(p || q)
      }
    }

    "plus" in {
      forAll(Table("v", randomElements[Double](100):_*)) { v =>
        val value = NumberValue(Complex.real(v))

        invokeFunction(SpecialFunctions.plus, value) mustBe value
      }
    }

    "add" in {
      forAll(Table(("l", "r"), randomPairs[Complex](100):_*)) { case (l, r) =>
        val left = NumberValue(l)
        val right = NumberValue(r)

        invokeFunction(SpecialFunctions.add, left, right) mustBe NumberValue(l + r)
      }
    }

    "minus" in {
      forAll(Table("v", randomElements[Complex](100):_*)) { v =>
        val value = NumberValue(v)

        invokeFunction(SpecialFunctions.minus, value) mustBe NumberValue(-v)
      }
    }

    "sub" in {
      forAll(Table(("l", "r"), randomPairs[Complex](100):_*)) { case (l, r) =>
        val left = NumberValue(l)
        val right = NumberValue(r)

        invokeFunction(SpecialFunctions.sub, left, right) mustBe NumberValue(l - r)
      }
    }

    "multi" in {
      forAll(Table(("l", "r"), randomPairs[Complex](100):_*)) { case (l, r) =>
        val left = NumberValue(l)
        val right = NumberValue(r)

        invokeFunction(SpecialFunctions.multi, left, right) mustBe NumberValue(l * r)
      }
    }

    "div" when {
      "succeed" in {
        val pairs = randomPairs[Complex](1000).filter(_._2 != Complex.zero)
        forAll(Table(("l", "r"), pairs:_*)) { case (l, r) =>
          val left = NumberValue(l)
          val right = NumberValue(r)

          invokeFunction(SpecialFunctions.div, left, right) mustBe NumberValue(l.by(r).value)
        }
      }

      "fail" in {
        val left = NumberValue(Complex.real(1337))
        val right = NumberValue(Complex.zero)

        expectFailure(SpecialFunctions.div, left, right) mustBe DivisionByZero
      }
    }

    "point" when {
      "succeed" in {
        forAll(Table(("x", "y"), randomPairs[Double](100):_*)) { case (x, y) =>
          val left = NumberValue(Complex.real(x))
          val right = NumberValue(Complex.real(y))

          invokeFunction(SpecialFunctions.point, left, right) mustBe PointValue(Point.cartesian(x, y))
        }
      }

      "fail" in {
        forAll(Table(("x", "y"), randomPairs[Double](100, _ != 0):_*)) { case (v, r) =>
          val x = NumberValue(Complex.imaginary(v))
          val y = NumberValue(Complex.imaginary(r))

          expectFailure(SpecialFunctions.point, x, y) mustBe IllegalParameter(SpecialFunctions.point, _0, x)
        }
      }
    }

    "vector" when {
      "succeed" in {
        forAll(Table(("dx", "dy"), randomPairs[Double](100):_*)) { case (dx, dy) =>
          val left = NumberValue(Complex.real(dx))
          val right = NumberValue(Complex.real(dy))

          invokeFunction(SpecialFunctions.vector, left, right) mustBe VectorValue(Vector.cartesian(dx, dy))
        }
      }

      "fail" in {
        forAll(Table(("x", "y"), randomPairs[Double](100, _ != 0):_*)) { case (v, r) =>
          val dx = NumberValue(Complex.imaginary(v))
          val dy = NumberValue(Complex.imaginary(r))

          expectFailure(SpecialFunctions.vector, dx, dy) mustBe IllegalParameter(SpecialFunctions.vector, _0, dx)
        }
      }
    }

    "angle" when {
      "succeed" in {
        forAll(Table("v", randomElements[Double](100):_*)) { v =>
          val value = NumberValue(Complex.real(v))

          invokeFunction(SpecialFunctions.angle, value) mustBe AngleValue(Angle.radians(v))
        }
      }

      "fail" in {
        forAll(Table("v", randomElements[Double](100):_*)) { v =>
          val value = NumberValue(Complex.imaginary(v))

          expectFailure(SpecialFunctions.angle, value) mustBe IllegalParameter(SpecialFunctions.angle, _0, value)
        }
      }
    }

    "color" when {
      "succeed" in {
        forAll(Table(("r", "g", "b"), randomTriples[Double](1000):_*)) { case (r, g, b) =>
          val red = NumberValue(Complex.real(r))
          val green = NumberValue(Complex.real(g))
          val blue = NumberValue(Complex.real(b))

          invokeFunction(SpecialFunctions.color, red, green, blue) mustBe ColorValue(Color.decimal(r, g, b))
        }
      }

      "fail" in {
        forAll(Table(("r", "g", "b"), randomTriples[Double](1000, _ != 0):_*)) { case (r, g, b) =>
          val red = NumberValue(Complex.imaginary(r))
          val green = NumberValue(Complex.imaginary(g))
          val blue = NumberValue(Complex.imaginary(b))

          expectFailure(SpecialFunctions.color, red, green, blue) mustBe IllegalParameter(SpecialFunctions.color, _0, red)
        }
      }
    }
  }

  private def invokeFunction[PN <: Nat](signature: FunctionSignature.Aux[PN], stack: Value*): Value = {
    val func = inside(new SpecialFunctionsModule[IO].apply()) { case Module.Prepared(objects) =>
      objects
        .find(_.signature == signature)
        .value
        .asInstanceOf[RuntimeFunction[IO]]
    }
    val ctx = stack.foldLeft(InterpreterContext.initial[IO])(_.pushValue(_))

    val io = for {
      resCtx <- func.invoke(testInterpreter, ctx)
      res    <- IO.fromEither(resCtx.pop.leftMap(InterpreterException))
    } yield res._1

    io.unsafeRunSync()
  }

  private def expectFailure[PN <: Nat](signature: FunctionSignature.Aux[PN], stack: Value*): InterpreterError = {
    val func = inside(new SpecialFunctionsModule[IO].apply()) { case Module.Prepared(objects) =>
      objects
        .find(_.signature == signature)
        .value
        .asInstanceOf[RuntimeFunction[IO]]
    }
    val ctx = stack.foldLeft(InterpreterContext.initial[IO])(_.pushValue(_))

    val res = func
      .invoke(testInterpreter, ctx)
      .attempt
      .unsafeRunSync()

    inside(res) { case Left(InterpreterException(error)) => error }
  }

  private val testInterpreter: Interpreter[IO, TPLCode] =
    (_, _) => IO.raiseError(new NotImplementedError("Unexpected call on dummy interpreter"))
}
