package com.github.scytrowski.sturtle.tpl.codegen

import cats.data.NonEmptyList
import com.github.scytrowski.sturtle.tpl.codegen.SyntaxTree.{Block, Expression}
import com.github.scytrowski.sturtle.tpl.fixture.CommonSpecLike
import com.github.scytrowski.sturtle.tpl.interpreter.TPLInstruction.{Branch, BranchCase, DefineFunction, ExitFunction, ExitLoop, Invoke, Loop, PopTo, PushFrom, PushValue}
import com.github.scytrowski.sturtle.tpl.interpreter.{BooleanValue, FunctionSignature, NumberValue, StringValue, TPLCode, TPLInstruction, VariableSignature, VoidValue}
import org.scalatest.Inside
import shapeless.Nat._3

class TPLCodeGeneratorTest extends CommonSpecLike with Inside {
  "TPLCodeGenerator" should {
    "generate TPL instructions" when {
      "node is Block" in {
        val block = SyntaxTree.Block(List(
          Expression.Name("a"),
          Expression.Name("b")
        ))

        generate(block) mustBe List(
          PushFrom(VariableSignature("a")),
          PushFrom(VariableSignature("b"))
        )
      }

      "node is Expression" when {
        "expression is Name" in {
          val name = "abcd123"

          generateSingleInstruction(Expression.Name(name)) mustBe PushFrom(VariableSignature(name))
        }

        "expression is Static" in {
          val value = NumberValue(-1.23456)

          generateSingleInstruction(Expression.Static(value)) mustBe PushValue(value)
        }

        "expression is FunctionCall" in {
          val name = "f"
          val parameters = List(
            Expression.Name("a"),
            Expression.Name("b"),
            Expression.Name("c")
          )

          generate(Expression.FunctionCall(Expression.Name(name), parameters)) mustBe List(
            PushFrom(VariableSignature("a")),
            PushFrom(VariableSignature("b")),
            PushFrom(VariableSignature("c")),
            Invoke(FunctionSignature(name, _3))
          )
        }
      }

      "node is FunctionDefinition" when {

        "return at the end of the body" in {
          val name = Expression.Name("f")
          val parameters = List(
            Expression.Name("a"),
            Expression.Name("b"),
            Expression.Name("c")
          )
          val definition = SyntaxTree.FunctionDefinition(
            name,
            parameters,
            Block(List(SyntaxTree.Break))
          )

          generateSingleInstruction(definition) mustBe DefineFunction(
            FunctionSignature(name.value, _3),
            TPLCode(
              parameters.toList.map(p => VariableSignature(p.value)).map(PopTo).reverse :+ ExitLoop:_*
            ).withExit(PushValue(VoidValue))
          )
        }

        "otherwise" in {
          val name = Expression.Name("g")
          val parameters = List(
            Expression.Name("d"),
            Expression.Name("e"),
            Expression.Name("f"),
          )
          val returnValue = StringValue("test")
          val definition = SyntaxTree.FunctionDefinition(
            name,
            parameters,
            Block(List(SyntaxTree.Return(Expression.Static(returnValue))))
          )

          generateSingleInstruction(definition) mustBe DefineFunction(
            FunctionSignature(name.value, _3),
            TPLCode(
              parameters.toList.map(p => VariableSignature(p.value)).map(PopTo).reverse:_*
            ).withExit(PushValue(returnValue))
          )
        }
      }

      "node is Branch" when {
        "single conditional branch" in {
          val branch = SyntaxTree.Branch(NonEmptyList.of(
            Case.Conditional(Expression.Name("a"), Block(List(SyntaxTree.Break)))
          ))

          generateSingleInstruction(branch) mustBe Branch(NonEmptyList.of(
            BranchCase(
              TPLCode.empty.withPush(PushFrom(VariableSignature("a"))),
              TPLCode(ExitLoop)
            )
          ))
        }

        "multiple conditional branches" in {
          val aReturnValue = NumberValue(1)
          val bReturnValue = StringValue("d")
          val cReturnValue = BooleanValue(false)
          val branch = SyntaxTree.Branch(NonEmptyList.of(
            Case.Conditional(Expression.Name("a"), Block(List(SyntaxTree.Return(Expression.Static(aReturnValue))))),
            Case.Conditional(Expression.Name("b"), Block(List(SyntaxTree.Return(Expression.Static(bReturnValue))))),
            Case.Conditional(Expression.Name("c"), Block(List(SyntaxTree.Return(Expression.Static(cReturnValue)))))
          ))

          generateSingleInstruction(branch) mustBe Branch(NonEmptyList.of(
            BranchCase(
              TPLCode.empty.withPush(PushFrom(VariableSignature("a"))),
              TPLCode.empty.withExit(PushValue(aReturnValue))
            ),
            BranchCase(
              TPLCode.empty.withPush(PushFrom(VariableSignature("b"))),
              TPLCode.empty.withExit(PushValue(bReturnValue))
            ),
            BranchCase(
              TPLCode.empty.withPush(PushFrom(VariableSignature("c"))),
              TPLCode.empty.withExit(PushValue(cReturnValue))
            )
          ))
        }

        "single conditional branch and default branch" in {
          val aReturnValue = VoidValue
          val defaultReturnValue = NumberValue(1337)
          val branch = SyntaxTree.Branch(NonEmptyList.of(
            Case.Conditional(Expression.Name("a"), Block(List(SyntaxTree.Return(Expression.Static(aReturnValue))))),
            Case.Default(Block(List(SyntaxTree.Return(Expression.Static(defaultReturnValue)))))
          ))

          generateSingleInstruction(branch) mustBe Branch(NonEmptyList.of(
            BranchCase(
              TPLCode.empty.withPush(PushFrom(VariableSignature("a"))),
              TPLCode.empty.withExit(PushValue(aReturnValue))
            ),
            BranchCase(
              TPLCode.empty.withPush(PushValue(BooleanValue(true))),
              TPLCode.empty.withExit(PushValue(defaultReturnValue))
            )
          ))
        }

        "multiple conditional branches and default branch" in {
          val aReturnValue = NumberValue(1)
          val bReturnValue = StringValue("d")
          val cReturnValue = BooleanValue(false)
          val defaultReturnValue = VoidValue
          val branch = SyntaxTree.Branch(NonEmptyList.of(
            Case.Conditional(Expression.Name("a"), Block(List(SyntaxTree.Return(Expression.Static(aReturnValue))))),
            Case.Conditional(Expression.Name("b"), Block(List(SyntaxTree.Return(Expression.Static(bReturnValue))))),
            Case.Conditional(Expression.Name("c"), Block(List(SyntaxTree.Return(Expression.Static(cReturnValue))))),
            Case.Default(Block(List(SyntaxTree.Return(Expression.Static(defaultReturnValue)))))
          ))

          generateSingleInstruction(branch) mustBe Branch(NonEmptyList.of(
            BranchCase(
              TPLCode.empty.withPush(PushFrom(VariableSignature("a"))),
              TPLCode.empty.withExit(PushValue(aReturnValue))
            ),
            BranchCase(
              TPLCode.empty.withPush(PushFrom(VariableSignature("b"))),
              TPLCode.empty.withExit(PushValue(bReturnValue))
            ),
            BranchCase(
              TPLCode.empty.withPush(PushFrom(VariableSignature("c"))),
              TPLCode.empty.withExit(PushValue(cReturnValue))
            ),
            BranchCase(
              TPLCode.empty.withPush(PushValue(BooleanValue(true))),
              TPLCode.empty.withExit(PushValue(defaultReturnValue))
            )
          ))
        }
      }

      "node is Loop" in {
        val loop = SyntaxTree.Loop(
          Expression.Name("a"),
          Block(List(SyntaxTree.Return(Expression.Name("b"))))
        )

        generateSingleInstruction(loop) mustBe Loop(
          TPLCode.empty.withPush(PushFrom(VariableSignature("a"))),
          TPLCode.empty.withExit(PushFrom(VariableSignature("b")))
        )
      }

      "node is Assignment" in {
        val variableName = "a"
        val valueName = "b"

        generate(SyntaxTree.Assignment(Expression.Name(variableName), Expression.Name(valueName))) mustBe List(
          PushFrom(VariableSignature(valueName)),
          PopTo(VariableSignature(variableName))
        )
      }

      "node is Return" in {
        val name = "a"
        val ret = SyntaxTree.Return(Expression.Name(name))

        generateSingleInstruction(ret) mustBe ExitFunction(PushFrom(VariableSignature(name)))
      }

      "node is Break" in {
        generateSingleInstruction(SyntaxTree.Break) mustBe ExitLoop
      }
    }
  }

  private def generateSingleInstruction(node: SyntaxTree): TPLInstruction =
    inside(generate(node)) { case instruction :: Nil => instruction }

  private def generate(node: SyntaxTree): List[TPLInstruction] = TPLCodeGenerator.generate(node).instructions
}
