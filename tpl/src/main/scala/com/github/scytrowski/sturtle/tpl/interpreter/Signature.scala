package com.github.scytrowski.sturtle.tpl.interpreter

import com.github.scytrowski.sturtle.tpl.types.Nat

sealed abstract class Signature {
  def name: String
}

final case class VariableSignature(name: String) extends Signature
sealed abstract class FunctionSignature extends Signature {
  type ParamsN <: Nat

  def paramsN: ParamsN
}
private final class FunctionSignatureImpl[N <: Nat](val name: String, val paramsN: N) extends FunctionSignature {
  override type ParamsN = N

  override def equals(obj: Any): Boolean =
    obj match {
      case other: FunctionSignature => name == other.name && paramsN == other.paramsN
      case _ => false
    }

  override def toString: String = s"$name/$paramsN"

  override def hashCode(): Int = (name, paramsN).hashCode()
}

object FunctionSignature {
  type Aux[PN <: Nat] = FunctionSignature { type ParamsN = PN }

  def apply[N <: Nat](name: String, pN: N): Aux[N] = new FunctionSignatureImpl[N](name, pN)
}