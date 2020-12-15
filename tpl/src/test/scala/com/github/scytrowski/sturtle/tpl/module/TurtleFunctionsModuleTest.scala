package com.github.scytrowski.sturtle.tpl.module

import cats.effect.IO
import cats.effect.concurrent.Ref
import cats.syntax.either._
import com.github.scytrowski.sturtle.core.TurtleCommand._
import com.github.scytrowski.sturtle.core.TurtleQuery.{GetAngle, GetFillColor, GetPenColor, GetPosition}
import com.github.scytrowski.sturtle.core.TurtleQueryAnswer.{AngleAnswer, FillColorAnswer, PenColorAnswer, PositionAnswer}
import com.github.scytrowski.sturtle.core.geometry.{Angle, Point}
import com.github.scytrowski.sturtle.core.graphics.Color
import com.github.scytrowski.sturtle.core.{TurtleCommand, TurtleController, TurtleQuery, TurtleQueryAnswer}
import com.github.scytrowski.sturtle.tpl.fixture.{EffectSpecLike, RandomnessFixture, TableFixture}
import com.github.scytrowski.sturtle.tpl.interpreter.InterpreterError.RealNumberExpected
import com.github.scytrowski.sturtle.tpl.interpreter._
import com.github.scytrowski.sturtle.tpl.types.Complex
import org.scalatest.Inside
import shapeless.Nat

class TurtleFunctionsModuleTest extends EffectSpecLike with RandomnessFixture with TableFixture with Inside {
  "TurtleFunctionsModule" when {
    "goto" in {
      forAll(Table("p", randomElements[Point](1000):_*)) { p =>
        val (result, data) = invokeFunction()(TurtleFunctions.goto, PointValue(p))

        result mustBe VoidValue
        data.commands mustBe List(MoveTo(p))
      }
    }

    "forward" when {
      "succeed" in {
        forAll(Table("r", randomElements[Double](1000):_*)) { r =>
          val (result, data) = invokeFunction()(TurtleFunctions.forward, NumberValue(Complex.real(r)))

          result mustBe VoidValue
          data.commands mustBe List(MoveForward(r))
        }
      }

      "fail" in {
        forAll(Table("r", randomElements[Double](1000, _ != 0):_*)) { r =>
          expectFailure()(TurtleFunctions.forward, NumberValue(Complex.imaginary(r))) mustBe RealNumberExpected
        }
      }
    }

    "backward" when {
      "succeed" in {
        forAll(Table("r", randomElements[Double](1000):_*)) { r =>
          val (result, data) = invokeFunction()(TurtleFunctions.backward, NumberValue(Complex.real(r)))

          result mustBe VoidValue
          data.commands mustBe List(MoveBackward(r))
        }
      }

      "fail" in {
        forAll(Table("r", randomElements[Double](1000):_*)) { r =>
          expectFailure()(TurtleFunctions.backward, NumberValue(Complex.imaginary(r))) mustBe RealNumberExpected
        }
      }
    }

    "pos" in {
      forAll(Table("p", randomElements[Point](1000):_*)) { p =>
        val (result, data) = invokeFunction(PositionAnswer(p))(TurtleFunctions.pos)

        result mustBe PointValue(p)
        data.queries mustBe List(GetPosition)
      }
    }

    "left" in {
      forAll(Table("a", randomElements[Angle](1000):_*)) { a =>
        val (result, data) = invokeFunction()(TurtleFunctions.left, AngleValue(a))

        result mustBe VoidValue
        data.commands mustBe List(RotateLeftBy(a))
      }
    }

    "right" in {
      forAll(Table("v", randomElements[Angle](1000):_*)) { a =>
        val (result, data) = invokeFunction()(TurtleFunctions.right, AngleValue(a))

        result mustBe VoidValue
        data.commands mustBe List(RotateRightBy(a))
      }
    }

    "angle" in {
      forAll(Table("a", randomElements[Angle](1000):_*)) { a =>
        val (result, data) = invokeFunction(AngleAnswer(a))(TurtleFunctions.angle)

        result mustBe AngleValue(a)
        data.queries mustBe List(GetAngle)
      }
    }

    "fill" in {
      val (result, data) = invokeFunction()(TurtleFunctions.fill)

      result mustBe VoidValue
      data.commands mustBe List(Fill)
    }

    "clear" in {
      val (result, data) = invokeFunction()(TurtleFunctions.clear)

      result mustBe VoidValue
      data.commands mustBe List(ClearPath)
    }

    "down" in {
      val (result, data) = invokeFunction()(TurtleFunctions.down)

      result mustBe VoidValue
      data.commands mustBe List(PenDown)
    }

    "up" in {
      val (result, data) = invokeFunction()(TurtleFunctions.up)

      result mustBe VoidValue
      data.commands mustBe List(PenUp)
    }

    "setPenColor" in {
      forAll(Table("c", randomElements[Color](1000):_*)) { c =>
        val (result, data) = invokeFunction()(TurtleFunctions.setPenColor, ColorValue(c))

        result mustBe VoidValue
        data.commands mustBe List(SetPenColor(c))
      }
    }

    "getPenColor" in {
      forAll(Table("c", randomElements[Color](1000):_*)) { c =>
        val (result, data) = invokeFunction(PenColorAnswer(c))(TurtleFunctions.getPenColor)

        result mustBe ColorValue(c)
        data.queries mustBe List(GetPenColor)
      }
    }

    "setFillColor" in {
      forAll(Table("c", randomElements[Color](1000):_*)) { c =>
        val (result, data) = invokeFunction()(TurtleFunctions.setFillColor, ColorValue(c))

        result mustBe VoidValue
        data.commands mustBe List(SetFillColor(c))
      }
    }

    "getFillColor" in {
      forAll(Table("c", randomElements[Color](1000):_*)) { c =>
        val (result, data) = invokeFunction(FillColorAnswer(c))(TurtleFunctions.getFillColor)

        result mustBe ColorValue(c)
        data.queries mustBe List(GetFillColor)
      }
    }
  }

  private def invokeFunction[PN <: Nat](answer: TurtleQueryAnswer = AngleAnswer(Angle.radians(0.123)))(signature: FunctionSignature.Aux[PN], stack: Value*): (Value, TestData) = {
    val ctx = stack.foldLeft(InterpreterContext.initial[IO])(_.pushValue(_))

    val io = for {
      dataRef  <- Ref.of[IO, TestData](TestData(answer = answer))
      module   = new TurtleFunctionsModule[IO].apply(testController(dataRef))
      result   <- {
        val func = inside(module) { case Module.Prepared(objects) =>
          objects
            .find(_.signature == signature)
            .value
            .asInstanceOf[RuntimeFunction[IO]]
        }
        func
          .invoke(testInterpreter, ctx)
          .flatMap(c => IO.fromEither(c.pop.leftMap(InterpreterException)))
      }
      data <- dataRef.get
    } yield result._1 -> data

    io.unsafeRunSync()
  }

  private def expectFailure[PN <: Nat](answer: TurtleQueryAnswer = AngleAnswer(Angle.radians(0.123)))(signature: FunctionSignature.Aux[PN], stack: Value*): InterpreterError = {
    val ctx = stack.foldLeft(InterpreterContext.initial[IO])(_.pushValue(_))

    val io = for {
      dataRef  <- Ref.of[IO, TestData](TestData(answer = answer))
      module   = new TurtleFunctionsModule[IO].apply(testController(dataRef))
      result   <- {
        val func = inside(module) { case Module.Prepared(objects) =>
          objects
            .find(_.signature == signature)
            .value
            .asInstanceOf[RuntimeFunction[IO]]
        }
        func
          .invoke(testInterpreter, ctx)
          .attempt
      }
    } yield result

    inside(io.unsafeRunSync()) { case Left(InterpreterException(error)) => error }
  }

  private def testController(dataRef: Ref[IO, TestData]): TurtleController[IO] =
    new TurtleController[IO] {
      override def run(command: TurtleCommand): IO[Unit] =
        dataRef.update(d => d.copy(commands = d.commands :+ command))

      override def execute(query: TurtleQuery): IO[TurtleQueryAnswer] =
        dataRef.modify { d =>
          d.copy(queries = d.queries :+ query) -> d.answer
        }
    }

  private val testInterpreter: Interpreter[IO, TPLCode] =
    (_, _) => IO.raiseError(new NotImplementedError("Unexpected call on dummy interpreter"))

  private case class TestData(commands: List[TurtleCommand] = Nil,
                              queries: List[TurtleQuery] = Nil,
                              answer: TurtleQueryAnswer)
}
