package com.github.scytrowski.sturtle.tpl.interpreter

import cats.data.NonEmptyList

sealed abstract class TPLInstruction

object TPLInstruction {
  sealed abstract class Push extends TPLInstruction
  final case class PushValue(value: Value) extends Push
  final case class PushFrom(from: VariableSignature) extends Push
  final case class Invoke(signature: FunctionSignature) extends Push
  final case class PopTo(to: VariableSignature) extends TPLInstruction
  final case class Branch(cases: NonEmptyList[BranchCase]) extends TPLInstruction
  final case class BranchCase(condition: TPLCode.WithPush, body: TPLCode)
  final case class Loop(condition: TPLCode.WithPush, body: TPLCode) extends TPLInstruction
  final case class DefineFunction(signature: FunctionSignature, body: TPLCode.WithExit) extends TPLInstruction
  case object ExitLoop extends TPLInstruction
  final case class ExitFunction(push: Push) extends TPLInstruction
}
