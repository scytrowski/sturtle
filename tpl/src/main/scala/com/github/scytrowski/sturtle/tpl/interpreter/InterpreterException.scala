package com.github.scytrowski.sturtle.tpl.interpreter

final case class InterpreterException(error: InterpreterError) extends Exception {
  override def getMessage: String = error.toString
}
