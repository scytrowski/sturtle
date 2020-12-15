package com.github.scytrowski.sturtle.tpl.module

import cats.MonadError
import cats.syntax.applicative._
import cats.syntax.functor._
import cats.syntax.flatMap._
import com.github.scytrowski.sturtle.tpl.interpreter.InterpreterError.RealNumberExpected
import com.github.scytrowski.sturtle.tpl.interpreter.{InterpreterError, InterpreterException, LogicalValue, NumericValue, PList, Value}
import com.github.scytrowski.sturtle.tpl.types.Complex
import shapeless.Nat
import shapeless.Nat.{_0, _1, _2}

abstract class NativeModuleOps[F[+_]: MonadError[*[_], Throwable]] {
  protected type FuncF[N <: Nat] = ParamsList[N] => F[Value]
  protected type ParamsList[N <: Nat] = PList.Aux[N]

  protected def unaryLogicalFunction(f: Boolean => Value): FuncF[_1] = params =>
    wrap {
      params.require[LogicalValue](_0)
        .map(_.logicalValue)
        .map(f)
    }

  protected def binaryLogicalFunction(f: (Boolean, Boolean) => Value): FuncF[_2] = params =>
    wrap {
      for {
        left <- params.require[LogicalValue](_0)
        right <- params.require[LogicalValue](_1)
      } yield f(left.logicalValue, right.logicalValue)
    }

  protected def unaryNumericFunction(f: Complex => Value): FuncF[_1] = params =>
    wrap {
      params.require[NumericValue](_0)
        .map(_.numericValue)
        .map(f)
    }

  protected def unaryNumericFunctionF(f: Complex => F[Value]): FuncF[_1] = params =>
    for {
      value <- wrap(params.require[NumericValue](_0))
      res   <- f(value.numericValue)
    } yield res


  protected def binaryNumericFunction(f: (Complex, Complex) => Value): FuncF[_2] =
    binaryNumericFunctionF { case (z, w) => f(z, w).pure }

  protected def binaryNumericFunctionF(f: (Complex, Complex) => F[Value]): FuncF[_2] = params =>
    for {
      left  <- wrap(params.require[NumericValue](_0))
      right <- wrap(params.require[NumericValue](_1))
      res   <- f(left.numericValue, right.numericValue)
    } yield res

  protected def requireReal(v: Complex): Either[InterpreterError, Double] =
    v.asReal
      .toRight(RealNumberExpected)

  protected def wrap[A](e: Either[InterpreterError, A]): F[A] =
    e match {
      case Right(a)    => a.pure
      case Left(error) => raiseError(error)
    }

  protected def raiseError(error: InterpreterError): F[Nothing] =
    MonadError[F, Throwable].raiseError(InterpreterException(error))
}
