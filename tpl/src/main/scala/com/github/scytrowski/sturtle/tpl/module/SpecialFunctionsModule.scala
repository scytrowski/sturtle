package com.github.scytrowski.sturtle.tpl.module

import cats.MonadError
import cats.syntax.applicative._
import cats.syntax.flatMap._
import cats.syntax.functor._
import com.github.scytrowski.sturtle.core.geometry.{Angle, Point, Vector}
import com.github.scytrowski.sturtle.core.graphics.Color
import com.github.scytrowski.sturtle.tpl.interpreter.InterpreterError.DivisionByZero
import com.github.scytrowski.sturtle.tpl.interpreter._
import shapeless.nat._

final class SpecialFunctionsModule[F[+_]: MonadError[*[_], Throwable]] extends NativeModuleOps[F] {
  def apply(): Module[F] =
    Module.Prepared(List(
      equal,
      notEqual,
      less,
      lessOrEqual,
      greater,
      greaterOrEqual,
      negate,
      and,
      or,
      plus,
      add,
      minus,
      sub,
      multi,
      div,
      point,
      vector,
      angle,
      color
    ))

  private val equal = RuntimeFunction(SpecialFunctions.equal).pure { params =>
    val left = params.at(_0)
    val right = params.at(_1)
    BooleanValue(left == right)
  }

  private val notEqual = RuntimeFunction(SpecialFunctions.notEqual).pure { params =>
    val left = params.at(_0)
    val right = params.at(_1)
    BooleanValue(left != right)
  }

  private val less = RuntimeFunction(SpecialFunctions.less).native(binaryNumericFunction { case (l, r) => BooleanValue(l < r) })

  private val lessOrEqual = RuntimeFunction(SpecialFunctions.lessOrEqual).native(binaryNumericFunction { case (l, r) => BooleanValue(l <= r) })

  private val greater = RuntimeFunction(SpecialFunctions.greater).native(binaryNumericFunction { case (l, r) => BooleanValue(l > r) })

  private val greaterOrEqual = RuntimeFunction(SpecialFunctions.greaterOrEqual).native(binaryNumericFunction { case (l, r) => BooleanValue(l >= r) })

  private val negate = RuntimeFunction(SpecialFunctions.negate).native(unaryLogicalFunction(b => BooleanValue(!b)))

  private val and = RuntimeFunction(SpecialFunctions.and).native(binaryLogicalFunction { case (l, r) => BooleanValue(l && r) })

  private val or = RuntimeFunction(SpecialFunctions.or).native(binaryLogicalFunction { case (l, r) => BooleanValue(l || r) })

  private val plus = RuntimeFunction(SpecialFunctions.plus).native(unaryNumericFunction(n => NumberValue(n)))

  private val add = RuntimeFunction(SpecialFunctions.add).native(binaryNumericFunction { case (l, r) => NumberValue(l + r) })

  private val minus = RuntimeFunction(SpecialFunctions.minus).native(unaryNumericFunction(n => NumberValue(-n)))

  private val sub = RuntimeFunction(SpecialFunctions.sub).native(binaryNumericFunction { case (l, r) => NumberValue(l - r) })

  private val multi = RuntimeFunction(SpecialFunctions.multi).native(binaryNumericFunction { case (l, r) => NumberValue(l * r) })

  private val div = RuntimeFunction(SpecialFunctions.div).native(binaryNumericFunctionF { case (l, r) =>
    l.by(r)
      .map(NumberValue)
      .fold[F[NumberValue]](raiseError(DivisionByZero))(_.pure)
  })

  private val point = RuntimeFunction(SpecialFunctions.point).native(binaryNumericFunctionF { case (x, y) =>
    for {
      xReal <- wrap(requireReal(x))
      yReal <- wrap(requireReal(y))
    } yield PointValue(Point.cartesian(xReal, yReal))
  })

  private val vector = RuntimeFunction(SpecialFunctions.vector).native(binaryNumericFunctionF { case (dx, dy) =>
    for {
      dxReal <- wrap(requireReal(dx))
      dyReal <- wrap(requireReal(dy))
    } yield VectorValue(Vector.cartesian(dxReal, dyReal))
  })

  private val angle = RuntimeFunction(SpecialFunctions.angle).native(unaryNumericFunctionF { v =>
    wrap(requireReal(v))
      .map(Angle.radians)
      .map(AngleValue)
  })

  private val color = RuntimeFunction(SpecialFunctions.color).native { params =>
    wrap {
      for {
        r <- params.require[NumericValue](_0).flatMap(v => requireReal(v.numericValue))
        g <- params.require[NumericValue](_1).flatMap(v => requireReal(v.numericValue))
        b <- params.require[NumericValue](_2).flatMap(v => requireReal(v.numericValue))
      } yield ColorValue(Color.decimal(r, g, b))
    }
  }
}
