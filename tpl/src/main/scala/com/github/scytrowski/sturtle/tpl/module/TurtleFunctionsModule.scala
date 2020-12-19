package com.github.scytrowski.sturtle.tpl.module

import cats.MonadError
import cats.syntax.applicative._
import cats.syntax.flatMap._
import cats.syntax.functor._
import com.github.scytrowski.sturtle.core.TurtleQuery.{GetAngle, GetFillColor, GetPenColor, GetPosition}
import com.github.scytrowski.sturtle.core.TurtleQueryAnswer.{AngleAnswer, FillColorAnswer, PenColorAnswer, PositionAnswer}
import com.github.scytrowski.sturtle.core._
import com.github.scytrowski.sturtle.tpl.interpreter.InterpreterError.InvalidAnswer
import com.github.scytrowski.sturtle.tpl.interpreter._
import com.github.scytrowski.sturtle.tpl.types.Nat._0
import com.github.scytrowski.sturtle.tpl.types._0

import scala.reflect.ClassTag

final class TurtleFunctionsModule[F[+_]: MonadError[*[_], Throwable]] extends NativeModuleOps[F] {
  def apply(controller: TurtleController[F]): Module[F] =
    Module.Prepared(List(
      goto(controller),
      forward(controller),
      backward(controller),
      pos(controller),
      left(controller),
      right(controller),
      angle(controller),
      fill(controller),
      clear(controller),
      down(controller),
      up(controller),
      setPenColor(controller),
      getPenColor(controller),
      setFillColor(controller),
      getFillColor(controller)
    ))

  private def goto(controller: TurtleController[F]) =
    RuntimeFunction(TurtleFunctions.goto).native { params =>
      for {
        to <- wrap(params.require[PointValue](_0))
        _  <- controller.run(TurtleCommand.MoveTo(to.value))
      } yield VoidValue
    }

  private def forward(controller: TurtleController[F]) =
    RuntimeFunction(TurtleFunctions.forward).native { params =>
      for {
        radius <- wrap(params.require[NumericValue](_0).flatMap(v => requireReal(v.numericValue)))
        _      <- controller.run(TurtleCommand.MoveForward(radius))
      } yield VoidValue
    }

  private def backward(controller: TurtleController[F]) =
    RuntimeFunction(TurtleFunctions.backward).native { params =>
      for {
        radius <- wrap(params.require[NumericValue](_0).flatMap(v => requireReal(v.numericValue)))
        _      <- controller.run(TurtleCommand.MoveBackward(radius))
      } yield VoidValue
    }

  private def pos(controller: TurtleController[F]) =
    RuntimeFunction(TurtleFunctions.pos).native {
      executeQuery[PositionAnswer](controller, GetPosition)(a => PointValue(a.position))
    }

  private def left(controller: TurtleController[F]) =
    RuntimeFunction(TurtleFunctions.left).native { params =>
      for {
        angle <- wrap(params.require[AngleValue](_0))
        _     <- controller.run(TurtleCommand.RotateLeftBy(angle.value))
      } yield VoidValue
    }

  private def right(controller: TurtleController[F]) =
    RuntimeFunction(TurtleFunctions.right).native { params =>
      for {
        angle <- wrap(params.require[AngleValue](_0))
        _     <- controller.run(TurtleCommand.RotateRightBy(angle.value))
      } yield VoidValue
    }

  private def angle(controller: TurtleController[F]) =
    RuntimeFunction(TurtleFunctions.angle).native {
      executeQuery[AngleAnswer](controller, GetAngle)(a => AngleValue(a.angle))
    }

  private def fill(controller: TurtleController[F]) =
    RuntimeFunction(TurtleFunctions.fill).native { _ =>
      controller
        .run(TurtleCommand.Fill)
        .as(VoidValue)
    }

  private def clear(controller: TurtleController[F]) =
    RuntimeFunction(TurtleFunctions.clear).native { _ =>
      controller
        .run(TurtleCommand.ClearPath)
        .as(VoidValue)
    }

  private def down(controller: TurtleController[F]) =
    RuntimeFunction(TurtleFunctions.down).native { _ =>
      controller
        .run(TurtleCommand.PenDown)
        .as(VoidValue)
    }

  private def up(controller: TurtleController[F]) =
    RuntimeFunction(TurtleFunctions.up).native { _ =>
      controller
        .run(TurtleCommand.PenUp)
        .as(VoidValue)
    }

  private def setPenColor(controller: TurtleController[F]) =
    RuntimeFunction(TurtleFunctions.setPenColor).native { params =>
      for {
        color <- wrap(params.require[ColorValue](_0))
        _     <- controller.run(TurtleCommand.SetPenColor(color.value))
      } yield VoidValue
    }

  private def getPenColor(controller: TurtleController[F]) =
    RuntimeFunction(TurtleFunctions.getPenColor).native {
      executeQuery[PenColorAnswer](controller, GetPenColor)(a => ColorValue(a.penColor))
    }

  private def setFillColor(controller: TurtleController[F]) =
    RuntimeFunction(TurtleFunctions.setFillColor).native { params =>
      for {
        color <- wrap(params.require[ColorValue](_0))
        _     <- controller.run(TurtleCommand.SetFillColor(color.value))
      } yield VoidValue
    }

  private def getFillColor(controller: TurtleController[F]) =
    RuntimeFunction(TurtleFunctions.getFillColor).native {
      executeQuery[FillColorAnswer](controller, GetFillColor)(a => ColorValue(a.fillColor))
    }

  private def executeQuery[A <: TurtleQueryAnswer](controller: TurtleController[F], query: TurtleQuery)(f: A => Value)(implicit classTag: ClassTag[A]): FuncF[_0] = _ =>
    controller
      .execute(query)
      .flatMap {
        case a: A    => f(a).pure
        case invalid => raiseError(InvalidAnswer(query, invalid))
      }
}
