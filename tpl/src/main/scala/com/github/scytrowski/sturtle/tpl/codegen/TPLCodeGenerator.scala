package com.github.scytrowski.sturtle.tpl.codegen

import com.github.scytrowski.sturtle.tpl.codegen.SyntaxTree.Expression
import com.github.scytrowski.sturtle.tpl.interpreter.TPLInstruction._
import com.github.scytrowski.sturtle.tpl.interpreter._
import com.github.scytrowski.sturtle.tpl.syntax.list._

object TPLCodeGenerator {
  def generate(node: SyntaxTree): TPLCode = node match {
    case block: SyntaxTree.Block => generateForBlock(block)
    case expr: Expression => generateForExpression(expr)
    case definition: SyntaxTree.FunctionDefinition => generateForFunctionDefinition(definition)
    case branch: SyntaxTree.Branch => generateForBranch(branch)
    case loop: SyntaxTree.Loop => generateForLoop(loop)
    case ret: SyntaxTree.Return => generateForReturn(ret)
    case SyntaxTree.Break => generateForBreak
  }

  private def generateForBlock(block: SyntaxTree.Block): TPLCode =
    generateAggregated(block.statements)

  private def generateForFunctionDefinition(definition: SyntaxTree.FunctionDefinition): TPLCode = {
    val signature = FunctionSignature(definition.name.value, definition.parameters.lengthN)
    val parameterSignatures = definition.parameters.map(p => VariableSignature(p.value))
    val popParameters = TPLCode(parameterSignatures.reverse.map(PopTo):_*)
    val code = (popParameters ++ generate(definition.body)).requireExit(PushValue(VoidValue))
    TPLCode(DefineFunction(signature, code))
  }

  private def generateForBranch(branch: SyntaxTree.Branch): TPLCode =
    TPLCode(Branch(branch.cases.map(generateForCase)))

  private def generateForCase(c: Case): BranchCase =
    c match {
      case conditional: Case.Conditional =>
        val condition = generateForExpression(conditional.condition)
        val body = generate(conditional.body)
        BranchCase(condition, body)
      case default: Case.Default =>
        val condition = TPLCode.empty.withPush(PushValue(BooleanValue(true)))
        val body = generate(default.body)
        BranchCase(condition, body)
    }

  private def generateForLoop(loop: SyntaxTree.Loop): TPLCode = {
    val condition = generateForExpression(loop.condition)
    val body = generate(loop.body)
    TPLCode(Loop(condition, body))
  }

  private def generateForReturn(ret: SyntaxTree.Return): TPLCode =
    generateForExpression(ret.expression).asExit

  private def generateForBreak: TPLCode = TPLCode(ExitLoop)

  private def generateForExpression(expr: SyntaxTree.Expression): TPLCode.WithPush =
    expr match {
      case Expression.Name(value) => TPLCode.empty.withPush(PushFrom(VariableSignature(value)))
      case Expression.Static(value) => TPLCode.empty.withPush(PushValue(value))
      case Expression.FunctionCall(name, parameters) =>
        val signature = FunctionSignature(name.value, parameters.lengthN)
        generateAggregated(parameters).withPush(Invoke(signature))
      case Expression.Assignment(name, value) =>
        val signature = VariableSignature(name.value)
        (generate(value) :+ PopTo(signature)).withPush(PushFrom(signature))
    }

  private def generateAggregated(nodes: List[SyntaxTree]): TPLCode =
    nodes
      .map(generate)
      .foldLeft(TPLCode.empty)(_ ++ _)
}
