package com.github.scytrowski.sturtle.tpl.module

import cats.MonadError
import cats.syntax.applicative._
import cats.syntax.flatMap._
import cats.syntax.functor._
import com.github.scytrowski.sturtle.tpl.interpreter._
import com.github.scytrowski.sturtle.tpl.types.Nat.{_0, _1, _2}
import com.github.scytrowski.sturtle.tpl.types.{Complex, Nat}

abstract class NativeModuleOps[F[+_]: MonadError[*[_], Throwable]] {
  protected type FuncF[N <: Nat] = ParamsList[N] => F[Value]
  protected type ParamsList[N <: Nat] = ParameterList.Aux[N]

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

  protected def unaryRealFunction(f: Double => Value): FuncF[_1] =
    unaryRealFunctionF(v => wrapEffect(f(v)))

  protected def unaryRealFunctionF(f: Double => F[Value]): FuncF[_1] = params =>
    for {
      value <- wrap(params.requireReal(_0))
      res   <- f(value)
    } yield res

  protected def unaryNumberFunction(f: Complex => Value): FuncF[_1] =
    unaryNumberFunctionF(v => wrapEffect(f(v)))

  protected def unaryNumberFunctionF(f: Complex => F[Value]): FuncF[_1] = params =>
    for {
      value <- wrap(params.require[NumberValue](_0))
      res   <- f(value.value)
    } yield res

  protected def unaryNumericFunction(f: Complex => Value): FuncF[_1] =
    unaryNumericFunctionF(v => wrapEffect(f(v)))

  protected def unaryNumericFunctionF(f: Complex => F[Value]): FuncF[_1] = params =>
    for {
      value <- wrap(params.require[NumericValue](_0))
      res   <- f(value.numericValue)
    } yield res

  protected def binaryRealFunction(f: (Double, Double) => Value): FuncF[_2] =
    binaryRealFunctionF { case (l, r) => wrapEffect(f(l, r)) }

  protected def binaryRealFunctionF(f: (Double, Double) => F[Value]): FuncF[_2] = params =>
    for {
      left  <- wrap(params.requireReal(_0))
      right <- wrap(params.requireReal(_1))
      res   <- f(left, right)
    } yield res

  protected def binaryNumberFunction(f: (Complex, Complex) => Value): FuncF[_2] =
    binaryNumberFunctionF { case (l, r) => wrapEffect(f(l, r)) }

  protected def binaryNumberFunctionF(f: (Complex, Complex) => F[Value]): FuncF[_2] = params =>
    for {
      left  <- wrap(params.require[NumericValue](_0))
      right <- wrap(params.require[NumericValue](_1))
      res   <- f(left.numericValue, right.numericValue)
    } yield res

  protected def binaryNumericFunction(f: (Complex, Complex) => Value): FuncF[_2] =
    binaryNumericFunctionF { case (z, w) => wrapEffect(f(z, w)) }

  protected def binaryNumericFunctionF(f: (Complex, Complex) => F[Value]): FuncF[_2] = params =>
    for {
      left  <- wrap(params.require[NumericValue](_0))
      right <- wrap(params.require[NumericValue](_1))
      res   <- f(left.numericValue, right.numericValue)
    } yield res

  protected def wrapEffect[A](a: => A): F[A] =
    MonadError[F, Throwable].catchNonFatal(a)

  protected def wrap[A](e: Either[InterpreterError, A]): F[A] =
    e match {
      case Right(a)    => a.pure
      case Left(error) => raiseError(error)
    }

  protected def raiseError(error: InterpreterError): F[Nothing] =
    MonadError[F, Throwable].raiseError(InterpreterException(error))
}
