package com.github.scytrowski.sturtle.tpl.module

import com.github.scytrowski.sturtle.tpl.interpreter.InterpreterError.IllegalParameter
import com.github.scytrowski.sturtle.tpl.interpreter.{FunctionSignature, InterpreterError, NumberValue, Value}
import com.github.scytrowski.sturtle.tpl.types.NatOps.Diff
import com.github.scytrowski.sturtle.tpl.types.{Complex, Nat, SizedList, Succ}

import scala.reflect.ClassTag

sealed abstract class ParameterList {
  type Size <: Nat

  def functionSignature: FunctionSignature.Aux[Size]

  def list: SizedList.Aux[Value, Size]

  final def requireNonZero(index: Nat)(implicit diff: Diff[Size, Succ[index.N]]): Either[InterpreterError, Complex] =
    requireMatching(index) { case n: NumberValue if !n.value.isZero => n.value }

  final def requireValidLogBase(index: Nat)(implicit diff: Diff[Size, Succ[index.N]]): Either[InterpreterError, Complex] =
    requireMatching(index) { case n: NumberValue if n.value.isValidLogBase => n.value }

  final def requireReal(index: Nat)(implicit diff: Diff[Size, Succ[index.N]]): Either[InterpreterError, Double] =
    requireMatching(index) { case n: NumberValue if n.value.isReal => n.value.real }

  final def require[R <: Value](index: Nat)
                               (implicit diff: Diff[Size, Succ[index.N]], reprTag: ClassTag[R]): Either[InterpreterError, R] =
    requireMatching(index) { case r: R => r }

  final def requireMatching[A](index: Nat)(pf: PartialFunction[Value, A])
                              (implicit diff: Diff[Size, Succ[index.N]]): Either[InterpreterError, A] = {
    val value = at(index)
    pf.lift(value) match {
      case Some(a) => Right(a)
      case None    => Left(IllegalParameter(functionSignature, index, value))
    }
  }

  final def at(index: Nat)(implicit diff: Diff[Size, Succ[index.N]]): Value = list.at(index)
}

object ParameterList {
  type Aux[S <: Nat] = ParameterList { type Size = S }

  def apply[S <: Nat](sign: FunctionSignature.Aux[S], l: SizedList.Aux[Value, S]): Aux[S] =
    new ParameterListImpl[S](sign, l)
}

private final class ParameterListImpl[S <: Nat](val functionSignature: FunctionSignature.Aux[S],
                                                val list: SizedList.Aux[Value, S]) extends ParameterList {
  override type Size = S
}
