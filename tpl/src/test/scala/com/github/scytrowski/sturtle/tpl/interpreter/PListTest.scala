package com.github.scytrowski.sturtle.tpl.interpreter

import com.github.scytrowski.sturtle.tpl.fixture.EffectSpecLike
import com.github.scytrowski.sturtle.tpl.interpreter.InterpreterError.InvalidValue
import shapeless.Nat.{_0, _1}

class PListTest extends EffectSpecLike {
  "PList" when {
    "at" should {
      "succeed" in {
        val value = NumberValue(1337)
        val list = testList(value)

        list.require[NumberValue](_0) mustBe Right(value)
      }

      "fail" in {
        val value = StringValue("abcd")
        val list = testList(value)

        list.require[NumberValue](_0) mustBe Left(InvalidValue(value))
      }
    }
  }

  private def testList(value: Value): PList.Aux[_1] = PList.wrap[_1](List(value))
}
