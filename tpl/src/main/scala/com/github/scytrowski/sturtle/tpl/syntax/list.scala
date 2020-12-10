package com.github.scytrowski.sturtle.tpl.syntax

import shapeless.{Nat, Succ}
import shapeless.Nat._0

object list {
  implicit class LengthN[A](list: List[A]) {
    def lengthN: Nat =
      list.foldLeft[Nat](_0) { case (n, _) => Succ[n.N]() }
  }
}
