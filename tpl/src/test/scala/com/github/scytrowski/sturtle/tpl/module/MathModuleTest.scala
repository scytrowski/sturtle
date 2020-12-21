package com.github.scytrowski.sturtle.tpl.module

import cats.effect.IO
import cats.syntax.either._
import com.github.scytrowski.sturtle.tpl.fixture.{EffectSpecLike, RandomnessFixture, TableFixture}
import com.github.scytrowski.sturtle.tpl.interpreter.InterpreterError.IllegalParameter
import com.github.scytrowski.sturtle.tpl.interpreter.{FunctionSignature, Interpreter, InterpreterContext, InterpreterError, InterpreterException, NumberValue, RuntimeFunction, RuntimeVariable, TPLCode, Value, VariableSignature}
import com.github.scytrowski.sturtle.tpl.types.Nat.{_0, _1}
import com.github.scytrowski.sturtle.tpl.types.{Complex, Nat}
import org.scalatest.Inside

class MathModuleTest extends EffectSpecLike with RandomnessFixture with TableFixture with Inside {
  "MathModule" when {
    "pi" should {
      "return correct value" in {
        retrieveConstant(MathConstants.pi) mustBe NumberValue(Complex.real(Math.PI))
      }
    }

    "e" should {
      "return correct value" in {
        retrieveConstant(MathConstants.e) mustBe NumberValue(Complex.real(Math.E))
      }
    }

    "i" should {
      "return correct value" in {
        retrieveConstant(MathConstants.i) mustBe NumberValue(Complex.unit)
      }
    }

    "re" should {
      "return correct value" in {
        forAll(Table("z", randomElements[Complex](1000):_*)) { z =>
          invokeFunction(MathFunctions.re, NumberValue(z)) mustBe NumberValue(Complex.real(z.real))
        }
      }
    }

    "im" should {
      "return correct value" in {
        forAll(Table("z", randomElements[Complex](1000):_*)) { z =>
          invokeFunction(MathFunctions.im, NumberValue(z)) mustBe NumberValue(Complex.real(z.imaginary))
        }
      }
    }

    "con" should {
      "return correct value" in {
        forAll(Table("z", randomElements[Complex](1000):_*)) { z =>
          invokeFunction(MathFunctions.con, NumberValue(z)) mustBe NumberValue(z.conjugate)
        }
      }
    }

    "abs" should {
      "return correct value" in {
        forAll(Table("z", randomElements[Complex](1000):_*)) { z =>
          invokeFunction(MathFunctions.abs, NumberValue(z)) mustBe NumberValue(Complex.real(z.abs))
        }
      }
    }

    "arg" should {
      "return correct value" in {
        forAll(Table("z", randomElements[Complex](1000):_*)) { z =>
          invokeFunction(MathFunctions.arg, NumberValue(z)) mustBe NumberValue(Complex.real(z.arg))
        }
      }
    }

    "floor" should {
      "return correct value" in {
        forAll(Table("v", randomElements[Double](1000):_*)) { v =>
          invokeFunction(MathFunctions.floor, NumberValue(Complex.real(v))) mustBe NumberValue(Complex.real(Math.floor(v)))
        }
      }

      "fail" when {
        "number is not real" in {
          forAll(Table("z", randomElements[Complex](100, !_.isReal):_*)) { z =>
            expectFailure(MathFunctions.floor, NumberValue(z)) mustBe IllegalParameter(MathFunctions.floor, _0, NumberValue(z))
          }
        }
      }
    }

    "ceil" should {
      "return correct value" in {
        forAll(Table("v", randomElements[Double](1000):_*)) { v =>
          invokeFunction(MathFunctions.ceil, NumberValue(Complex.real(v))) mustBe NumberValue(Complex.real(Math.ceil(v)))
        }
      }

      "fail" when {
        "number is not real" in {
          forAll(Table("z", randomElements[Complex](100, !_.isReal):_*)) { z =>
            expectFailure(MathFunctions.ceil, NumberValue(z)) mustBe IllegalParameter(MathFunctions.ceil, _0, NumberValue(z))
          }
        }
      }
    }

    "min" should {
      "return correct value" in {
        forAll(Table(("z", "w"), randomElements[(Double, Double)](1000):_*)) { case (z, w) =>
          invokeFunction(MathFunctions.min, NumberValue(Complex.real(z)), NumberValue(Complex.real(w))) mustBe NumberValue(Complex.real(Math.min(z, w)))
        }
      }

      "fail" when {
        "first parameter is invalid" in {
          forAll(Table(("z", "w"), randomElements[(Complex, Double)](100, { case (z, _) => !z.isReal }):_*)) { case (z, w) =>
            expectFailure(MathFunctions.min, NumberValue(z), NumberValue(Complex.real(w))) mustBe IllegalParameter(MathFunctions.min, _0, NumberValue(z))
          }
        }

        "second parameter is invalid" in {
          forAll(Table(("z", "w"), randomElements[(Double, Complex)](100, { case (_, w) => !w.isReal }):_*)) { case (z, w) =>
            expectFailure(MathFunctions.min, NumberValue(Complex.real(z)), NumberValue(w)) mustBe IllegalParameter(MathFunctions.min, _1, NumberValue(w))
          }
        }
      }
    }

    "max" should {
      "return correct value" in {
        forAll(Table(("z", "w"), randomElements[(Double, Double)](1000):_*)) { case (z, w) =>
          invokeFunction(MathFunctions.max, NumberValue(Complex.real(z)), NumberValue(Complex.real(w))) mustBe NumberValue(Complex.real(Math.max(z, w)))
        }
      }

      "fail" when {
        "first parameter is invalid" in {
          forAll(Table(("z", "w"), randomElements[(Complex, Double)](100, { case (z, _) => !z.isReal }):_*)) { case (z, w) =>
            expectFailure(MathFunctions.max, NumberValue(z), NumberValue(Complex.real(w))) mustBe IllegalParameter(MathFunctions.max, _0, NumberValue(z))
          }
        }

        "second parameter is invalid" in {
          forAll(Table(("z", "w"), randomElements[(Double, Complex)](100, { case (_, w) => !w.isReal }):_*)) { case (z, w) =>
            expectFailure(MathFunctions.max, NumberValue(Complex.real(z)), NumberValue(w)) mustBe IllegalParameter(MathFunctions.max, _1, NumberValue(w))
          }
        }
      }
    }

    "pow" should {
      "return correct value" in {
        forAll(Table(("z", "w"), randomPairs[Complex](1000):_*)) { case (z, w) =>
          invokeFunction(MathFunctions.pow, NumberValue(z), NumberValue(w)) mustBe NumberValue(z ** w)
        }
      }
    }

    "exp" should {
      "return correct value" in {
        forAll(Table("z", randomElements[Complex](1000):_*)) { z =>
          invokeFunction(MathFunctions.exp, NumberValue(z)) mustBe NumberValue(z.exp)
        }
      }
    }

    "log" should {
      "return correct value" in {
        forAll(Table(("a", "b"), randomElements[(Complex, Complex)](1000, { case (a, b) => !a.isZero && b.isValidLogBase }):_*)) { case (a, b) =>
          invokeFunction(MathFunctions.log, NumberValue(a), NumberValue(b)) mustBe NumberValue(a.log(b).value)
        }
      }

      "fail" when {
        "a is 0" in {
          forAll(Table("b", randomElements[Complex](100, _.isValidLogBase):_*)) { b =>
            expectFailure(MathFunctions.log, NumberValue(Complex.zero), NumberValue(b)) mustBe IllegalParameter(MathFunctions.log, _0, NumberValue(Complex.zero))
          }
        }

        "base is non positive" in {
          forAll(Table(("a", "b"), randomElements[(Complex, Double)](100, { case (a, b) => !a.isZero && b <= 0 }):_*)) { case (a, b) =>
            expectFailure(MathFunctions.log, NumberValue(a), NumberValue(Complex.real(b))) mustBe IllegalParameter(MathFunctions.log, _1, NumberValue(Complex.real(b)))
          }
        }

        "base is 1" in {
          forAll(Table("a", randomElements[Complex](100, !_.isZero):_*)) { a =>
            expectFailure(MathFunctions.log, NumberValue(a), NumberValue(Complex.one)) mustBe IllegalParameter(MathFunctions.log, _1, NumberValue(Complex.one))
          }
        }
      }
    }

    "ln" should {
      "return correct value" in {
        forAll(Table("z", randomElements[Complex](1000, !_.isZero):_*)) { z =>
          invokeFunction(MathFunctions.ln, NumberValue(z)) mustBe NumberValue(z.ln.value)
        }
      }

      "fail" when {
        "a is 0" in {
          expectFailure(MathFunctions.ln, NumberValue(Complex.zero)) mustBe IllegalParameter(MathFunctions.ln, _0, NumberValue(Complex.zero))
        }
      }
    }

    "sin" should {
      "return correct value" in {
        forAll(Table("z", randomElements[Complex](1000):_*)) { z =>
          invokeFunction(MathFunctions.sin, NumberValue(z)) mustBe NumberValue(z.sin)
        }
      }
    }

    "cos" should {
      "return correct value" in {
        forAll(Table("z", randomElements[Complex](1000):_*)) { z =>
          invokeFunction(MathFunctions.cos, NumberValue(z)) mustBe NumberValue(z.cos)
        }
      }
    }
  }

  private def retrieveConstant(signature: VariableSignature): Value =
    inside(new MathModule[IO].apply()) { case Module.Prepared(objects) =>
      objects
        .find(_.signature == signature)
        .value
        .asInstanceOf[RuntimeVariable]
        .value
    }

  private def invokeFunction[PN <: Nat](signature: FunctionSignature.Aux[PN], stack: Value*): Value = {
    val func = inside(new MathModule[IO].apply()) { case Module.Prepared(objects) =>
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
    val func = inside(new MathModule[IO].apply()) { case Module.Prepared(objects) =>
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
