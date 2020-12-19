package com.github.scytrowski.sturtle.tpl.interpreter

trait Stack[A] {
  type Self <: Stack[A]

  def push(a: A): Self

  def pop: Option[(A, Self)]

  final def merge(other: Stack[A]): Stack[A] = MergedStack(this, other)

  final def asLazyList: LazyList[A] = LazyList.unfold(this)(_.pop)
}

object Stack {
  def empty[A]: Stack[A] = Stack()

  def apply[A](elements: A*): Stack[A] = ListBasedStack(elements.toList)
}

private final case class ListBasedStack[A](elements: List[A]) extends Stack[A] {
  override type Self = ListBasedStack[A]

  override def push(a: A): ListBasedStack[A] = copy(elements = a :: elements)

  override def pop: Option[(A, ListBasedStack[A])] =
    elements match {
      case head :: tail => Some(head -> copy(elements = tail))
      case _ => None
    }
}

private final case class MergedStack[A](inner: Stack[A], outer: Stack[A]) extends Stack[A] {
  override type Self = MergedStack[A]

  override def push(a: A): MergedStack[A] = copy(outer = outer.push(a))

  override def pop: Option[(A, MergedStack[A])] =
    outer.pop
      .map { case (a, nStack) => a -> copy(outer = nStack) }
      .orElse {
        inner.pop
          .map { case (a, nStack) => a -> copy(inner = nStack) }
      }
}
