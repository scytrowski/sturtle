package com.github.scytrowski.sturtle.tpl.module

import cats.MonadError
import cats.syntax.applicative._
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

  private val div = RuntimeFunction(SpecialFunctions.div).native(binaryNumericFunctionF {
    case (l, r) if r != 0 => NumberValue(l / r).pure[F]
    case _ => raiseError(DivisionByZero)
  })

  private val point = RuntimeFunction(SpecialFunctions.point).native(binaryNumericFunction { case (x, y) => PointValue(Point.cartesian(x, y)) })

  private val vector = RuntimeFunction(SpecialFunctions.vector).native(binaryNumericFunction { case (dx, dy) => VectorValue(Vector.cartesian(dx, dy)) })

  private val angle = RuntimeFunction(SpecialFunctions.angle).native(unaryNumericFunction(v => AngleValue(Angle.radians(v))))

  private val color = RuntimeFunction(SpecialFunctions.color).native { params =>
    wrap {
      for {
        r <- params.require[NumericValue](_0)
        g <- params.require[NumericValue](_1)
        b <- params.require[NumericValue](_2)
      } yield ColorValue(Color.decimal(r.numericValue, g.numericValue, b.numericValue))
    }
  }
}
