package com.github.scytrowski.sturtle.tpl.interpreter

trait Stack[A] {
  type Self <: Stack[A]

  def push(a: A): Self

  def pop: Option[(A, Self)]
}

object Stack {
  def empty[A]: Stack[A] = ListBasedStack(List.empty)
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
