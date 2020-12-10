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
import com.github.scytrowski.sturtle.tpl.fixture.EffectSpecLike
import com.github.scytrowski.sturtle.tpl.interpreter._
import org.scalatest.Inside
import shapeless.Nat

class TurtleFunctionsModuleTest extends EffectSpecLike with Inside {
  "TurtleFunctionsModule" when {
    "goto" in {
      val to = Point(1, 2)

      val (result, data) = invokeFunction()(TurtleFunctions.goto, PointValue(to))

      result mustBe VoidValue
      data.commands mustBe List(MoveTo(to))
    }

    "forward" in {
      val radius = 17.19

      val (result, data) = invokeFunction()(TurtleFunctions.forward, NumberValue(radius))

      result mustBe VoidValue
      data.commands mustBe List(MoveForward(radius))
    }

    "backward" in {
      val radius = 913.737

      val (result, data) = invokeFunction()(TurtleFunctions.backward, NumberValue(radius))

      result mustBe VoidValue
      data.commands mustBe List(MoveBackward(radius))
    }

    "pos" in {
      val pos = Point(15, -17)

      val (result, data) = invokeFunction(PositionAnswer(pos))(TurtleFunctions.pos)

      result mustBe PointValue(pos)
      data.queries mustBe List(GetPosition)
    }

    "left" in {
      val angle = Angle(2.1972)

      val (result, data) = invokeFunction()(TurtleFunctions.left, AngleValue(angle))

      result mustBe VoidValue
      data.commands mustBe List(RotateLeftBy(angle))
    }

    "right" in {
      val angle = Angle(1.0003)

      val (result, data) = invokeFunction()(TurtleFunctions.right, AngleValue(angle))

      result mustBe VoidValue
      data.commands mustBe List(RotateRightBy(angle))
    }

    "angle" in {
      val angle = Angle(0.123)

      val (result, data) = invokeFunction(AngleAnswer(angle))(TurtleFunctions.angle)

      result mustBe AngleValue(angle)
      data.queries mustBe List(GetAngle)
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
      val color = Color.rgb(17, 2, 78)

      val (result, data) = invokeFunction()(TurtleFunctions.setPenColor, ColorValue(color))

      result mustBe VoidValue
      data.commands mustBe List(SetPenColor(color))
    }

    "getPenColor" in {
      val color = Color.rgb(25, 50, 75)

      val (result, data) = invokeFunction(PenColorAnswer(color))(TurtleFunctions.getPenColor)

      result mustBe ColorValue(color)
      data.queries mustBe List(GetPenColor)
    }

    "setFillColor" in {
      val color = Color.rgb(39, 15, 111)

      val (result, data) = invokeFunction()(TurtleFunctions.setFillColor, ColorValue(color))

      result mustBe VoidValue
      data.commands mustBe List(SetFillColor(color))
    }

    "getFillColor" in {
      val color = Color.rgb(100, 150, 200)

      val (result, data) = invokeFunction(FillColorAnswer(color))(TurtleFunctions.getFillColor)

      result mustBe ColorValue(color)
      data.queries mustBe List(GetFillColor)
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
