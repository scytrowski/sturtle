package com.github.scytrowski.sturtle.tpl.interpreter

import com.github.scytrowski.sturtle.tpl.interpreter.TPLCode.{WithExit, WithPush}
import com.github.scytrowski.sturtle.tpl.interpreter.TPLInstruction.{ExitFunction, Push}

sealed abstract class TPLCode {
  def instructions: List[TPLInstruction]

  final def headOption: Option[TPLInstruction] = instructions.headOption
  final def tail: TPLCode = TPLCode(instructions.tail:_*)

  final def :+(instruction: TPLInstruction): TPLCode = append(instruction)
  final def append(instruction: TPLInstruction): TPLCode = TPLCode(instructions :+ instruction:_*)

  final def ++(other: TPLCode): TPLCode = extend(other)
  final def extend(other: TPLCode): TPLCode = TPLCode(instructions ++ other.instructions:_*)

  final def requirePush(default: => Push): WithPush =
    this match {
      case withPush: WithPush => withPush
      case _ => withPush(default)
    }

  final def requireExit(default: => Push): WithExit =
    this match {
      case withExit: WithExit => withExit
      case withPush: WithPush => withPush.asExit
      case _ => withExit(default)
    }

  final def withPush(push: Push): WithPush = WithPush(instructions, push)

  final def withExit(push: Push): WithExit = WithExit(instructions, ExitFunction(push))
}

object TPLCode {
  val empty: TPLCode = TPLCode()

  def apply(ins: TPLInstruction*): TPLCode =
    ins.lastOption match {
      case Some(push: Push) => WithPush(ins.dropRight(1).toList, push)
      case Some(exit: ExitFunction) => WithExit(ins.dropRight(1).toList, exit)
      case _ => Regular(ins.toList)
    }

  final case class Regular private(instructions: List[TPLInstruction]) extends TPLCode

  final case class WithPush private(prePush: List[TPLInstruction], push: Push) extends TPLCode {
    override def instructions: List[TPLInstruction] = prePush :+ push

    def asExit: WithExit = WithExit(prePush, ExitFunction(push))
  }

  final case class WithExit private(preExit: List[TPLInstruction], exit: ExitFunction) extends TPLCode {
    override def instructions: List[TPLInstruction] = preExit :+ exit
  }
}
