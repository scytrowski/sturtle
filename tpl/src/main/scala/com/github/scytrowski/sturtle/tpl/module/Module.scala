package com.github.scytrowski.sturtle.tpl.module

import com.github.scytrowski.sturtle.tpl.interpreter.{RuntimeObject, TPLCode}

sealed abstract class Module[+F[_]]

object Module {
  final case class Raw(code: TPLCode) extends Module[Nothing]
  final case class Prepared[F[_]](objects: List[RuntimeObject[F]]) extends Module
}
