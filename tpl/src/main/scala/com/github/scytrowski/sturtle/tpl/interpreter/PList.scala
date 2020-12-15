package com.github.scytrowski.sturtle.tpl.interpreter

import com.github.scytrowski.sturtle.tpl.interpreter.InterpreterError.{InvalidValue, RealNumberExpected}
import shapeless.ops.nat.{Diff, ToInt}
import shapeless.{Nat, Sized, Succ}

import scala.reflect.ClassTag

sealed abstract class PList {
  type ParamsN <: Nat

  def at(index: Nat)(implicit diff: Diff[ParamsN, Succ[index.N]], toInt: ToInt[index.N]): Value

  final def requireReal(index: Nat)
                       (implicit diff: Diff[ParamsN, Succ[index.N]], toInt: ToInt[index.N], reprTag: ClassTag[NumberValue]): Either[InterpreterError, Double] =
    require[NumberValue](index)
      .flatMap(_.value.asReal.toRight(RealNumberExpected))

  final def require[R <: Value](index: Nat)
                               (implicit diff: Diff[ParamsN, Succ[index.N]], toInt: ToInt[index.N], reprTag: ClassTag[R]): Either[InterpreterError, R] =
    at(index) match {
      case r: R    => Right(r)
      case invalid => Left(InvalidValue(invalid))
    }
}

object PList {
  type Aux[PN <: Nat] = PList { type ParamsN = PN }

  def wrap[PN <: Nat](params: List[Value]): Aux[PN] =
    fromSized(Sized.wrap[List[Value], PN](params))

  def fromSized[PN <: Nat](sized: Sized[List[Value], PN]): Aux[PN] =
    new PList {
      override type ParamsN = PN

      override def at(index: Nat)(implicit diff: Diff[PN, Succ[index.N]], toInt: ToInt[index.N]): Value = sized.at(index)
    }
}
