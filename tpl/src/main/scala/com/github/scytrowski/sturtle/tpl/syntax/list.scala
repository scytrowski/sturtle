package com.github.scytrowski.sturtle.tpl.syntax

import com.github.scytrowski.sturtle.tpl.types.{Nat, Succ}
import com.github.scytrowski.sturtle.tpl.types.Nat._0

object list {
  implicit class LengthN[A](list: List[A]) {
    def lengthN: Nat =
      list.foldLeft[Nat](_0) { case (n, _) => Succ(n) }
  }
}
