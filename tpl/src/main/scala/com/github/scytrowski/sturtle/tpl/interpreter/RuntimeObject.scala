package com.github.scytrowski.sturtle.tpl.interpreter

sealed abstract class RuntimeObject {
  type S <: Signature

  def signature: S
}

object RuntimeObject {
  type Aux[Sig <: Signature] = RuntimeObject { type S = Sig }
}

final case class RuntimeVariable(signature: VariableSignature, value: Value) extends RuntimeObject {
  override type S = VariableSignature
}

final case class RuntimeFunction(signature: FunctionSignature, code: TPLCode.WithExit) extends RuntimeObject {
  override type S = FunctionSignature
}
