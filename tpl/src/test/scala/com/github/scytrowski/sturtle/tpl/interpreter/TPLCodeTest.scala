package com.github.scytrowski.sturtle.tpl.interpreter

import com.github.scytrowski.sturtle.tpl.fixture.CommonSpecLike
import com.github.scytrowski.sturtle.tpl.interpreter.TPLCode.{Regular, WithExit, WithPush}
import com.github.scytrowski.sturtle.tpl.interpreter.TPLInstruction.{ExitFunction, ExitLoop, PopTo, PushValue}
import org.scalatest.Inside

class TPLCodeTest extends CommonSpecLike with Inside {
  "TPLCode" when {
    "create" when {
      "with push" in {
        val push = PushValue(StringValue("defghi"))
        val instructions = List(
          ExitLoop,
          push
        )

        inside(TPLCode(instructions:_*)) { case withPush: WithPush =>
          withPush.push mustBe push
          withPush.prePush mustBe instructions.dropRight(1)
        }
      }

      "with exit" in {
        val exit = ExitFunction(PushValue(StringValue("defghi")))
        val instructions = List(
          PopTo(VariableSignature("b")),
          exit
        )

        inside(TPLCode(instructions:_*)) { case withExit: WithExit =>
          withExit.exit mustBe exit
          withExit.preExit mustBe instructions.dropRight(1)
        }
      }

      "regular" in {
        val instructions = List(
          PopTo(VariableSignature("c")),
          ExitLoop
        )

        inside(TPLCode(instructions:_*)) { case regular: Regular =>
          regular.instructions mustBe instructions
        }
      }
    }
  }
}
