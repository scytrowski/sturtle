package com.github.scytrowski.sturtle.tpl.interpreter

import com.github.scytrowski.sturtle.tpl.fixture.CommonSpecLike
import org.scalatest.OptionValues

class StackTest extends CommonSpecLike with OptionValues {
  "Stack" when {
    "push" should {
      "push elements onto stack" in {
        val elements = 1.to(100).toList
        val stack = elements.foldLeft(Stack.empty[Int])(_.push(_))

        stack mustBe ListBasedStack(elements.reverse)
      }
    }

    "pop" when {
      "non empty" in {
        val value = "abcd"
        val stack = ListBasedStack(List(value))

        val (top, updatedStack) = stack.pop.value

        top mustBe value
        updatedStack mustBe Stack.empty
      }

      "empty" in {
        Stack.empty.pop.isEmpty mustBe true
      }
    }
  }
}
