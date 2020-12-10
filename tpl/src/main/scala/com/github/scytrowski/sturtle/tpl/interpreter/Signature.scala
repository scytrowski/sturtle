package com.github.scytrowski.sturtle.tpl.interpreter

import shapeless.Nat

sealed abstract class Signature {
  def name: String
}

final case class VariableSignature(name: String) extends Signature
sealed abstract class FunctionSignature(val name: String, val paramsN: Nat) extends Signature {
  type ParamsN <: Nat

  override def equals(obj: Any): Boolean =
    obj match {
      case other: FunctionSignature => name == other.name && paramsN == other.paramsN
      case _ => false
    }

  override def hashCode(): Int = (name, paramsN).hashCode()
}

object FunctionSignature {
  type Aux[PN <: Nat] = FunctionSignature { type ParamsN = PN }

  def apply(name: String, pN: Nat): Aux[pN.N] = new FunctionSignature(name, pN) {
    override type ParamsN = pN.N
  }
}