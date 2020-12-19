package com.github.scytrowski.sturtle.tpl.types

import com.github.scytrowski.sturtle.tpl.types.Nat._0
import shapeless.Lazy

sealed abstract class Nat {
  type N <: Nat

  def value: Int

  final override def toString: String = value.toString
}

object Nat {
  type Aux[T <: Nat] = Nat { type N = T }

  val _0: _0 = new _0 {}

  val _1: _1 = Succ(_0)
  type _1 = Succ[_0]

  val _2: _2 = Succ(_1)
  type _2 = Succ[_1]

  val _3: _3 = Succ(_2)
  type _3 = Succ[_2]

  val _4: _4 = Succ(_3)
  type _4 = Succ[_3]

  val _5: _5 = Succ(_4)
  type _5 = Succ[_4]

  val _6: _6 = Succ(_5)
  type _6 = Succ[_5]
}

object NatOps {
  sealed abstract class Instance[N <: Nat] {
    def out: N
  }

  object Instance {
    def apply[N <: Nat](implicit instance: Instance[N]): Instance[N] = instance

    implicit val zeroInstance: Instance[_0] = new Instance[_0] {
      override def out: _0 = _0
    }

    implicit def succInstance[N <: Nat](implicit nInstance: Lazy[Instance[N]]): Instance[Succ[N]] = new Instance[Succ[N]] {
      override def out: Succ[N] = Succ(nInstance.value.out)
    }
  }

  sealed abstract class Diff[N <: Nat, M <: Nat] {
    type Out <: Nat
  }

  object Diff {
    type Aux[A <: Nat, B <: Nat, C <: Nat] = Diff[A, B] { type Out = C }

    implicit def nDiffZero[N <: Nat]: Aux[N, _0, N] = new Diff[N, _0] {
      override type Out = N
    }
    implicit def succNDiffSuccM[N <: Nat, M <: Nat, O <: Nat](implicit diff: Aux[N, M, O]): Aux[Succ[N], Succ[M], O] =
      new Diff[Succ[N], Succ[M]] {
        override type Out = O
      }
  }
}

sealed abstract class _0 extends Nat {
  override type N = _0

  override val value: Int = 0
}

final case class Succ[P <: Nat](predecessor: P) extends Nat {
  override type N = Succ[P]

  override def value: Int = predecessor.value + 1
}