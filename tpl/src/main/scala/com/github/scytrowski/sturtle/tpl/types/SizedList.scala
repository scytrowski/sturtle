package com.github.scytrowski.sturtle.tpl.types

import com.github.scytrowski.sturtle.tpl.types.NatOps.Diff

sealed abstract class SizedList[+A] private(elements: List[A]) {
  import SizedList._

  type Size <: Nat

  final def at(index: Nat)(implicit diff: Diff[Size, Succ[index.N]]): A = elements(index.value)

  final def take(count: Nat)(implicit diff: Diff[Size, count.N]): Aux[A, count.N] =
    wrap(elements.take(count.value))

  final def toList: List[A] = elements
}

object SizedList {
  type Aux[+A, S <: Nat] = SizedList[A] { type Size = S }

  def wrap[A, N <: Nat](elements: List[A]): Aux[A, N] =
    new SizedList[A](elements) {
      override type Size = N
    }
}
