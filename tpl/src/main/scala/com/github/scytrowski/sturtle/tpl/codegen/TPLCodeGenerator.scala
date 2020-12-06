package com.github.scytrowski.sturtle.tpl.codegen

import com.github.scytrowski.sturtle.tpl.codegen.SyntaxTree.Expression
import com.github.scytrowski.sturtle.tpl.interpreter.TPLInstruction.{Branch, BranchCase, ExitLoop, DefineFunction, Invoke, Loop, PopTo, PushFrom, PushValue}
import com.github.scytrowski.sturtle.tpl.interpreter.{FunctionSignature, TPLCode, VariableSignature}
import com.github.scytrowski.sturtle.tpl.interpreter.Value.{BooleanValue, VoidValue}

object TPLCodeGenerator {
  def generate(node: SyntaxTree): TPLCode = node match {
    case block: SyntaxTree.Block => generateForBlock(block)
    case expr: Expression => generateForExpression(expr)
    case definition: SyntaxTree.FunctionDefinition => generateForFunctionDefinition(definition)
    case branch: SyntaxTree.Branch => generateForBranch(branch)
    case loop: SyntaxTree.Loop => generateForLoop(loop)
    case assignment: SyntaxTree.Assignment => generateForAssignment(assignment)
    case ret: SyntaxTree.Return => generateForReturn(ret)
    case SyntaxTree.Break => generateForBreak
  }

  private def generateForBlock(block: SyntaxTree.Block): TPLCode =
    generateAggregated(block.statements)

  private def generateForFunctionDefinition(definition: SyntaxTree.FunctionDefinition): TPLCode = {
    val signature = FunctionSignature(definition.name.value, definition.parameters.length)
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

  private def generateForAssignment(assignment: SyntaxTree.Assignment): TPLCode = {
    val signature = VariableSignature( assignment.variableName.value)
    generate(assignment.value) :+ PopTo(signature)
  }

  private def generateForReturn(ret: SyntaxTree.Return): TPLCode =
    generateForExpression(ret.expression).asExit

  private def generateForBreak: TPLCode = TPLCode(ExitLoop)

  private def generateForExpression(expr: SyntaxTree.Expression): TPLCode.WithPush =
    expr match {
      case Expression.Name(value) => TPLCode.empty.withPush(PushFrom(VariableSignature(value)))
      case Expression.Static(value) => TPLCode.empty.withPush(PushValue(value))
      case Expression.FunctionCall(name, parameters) =>
        val signature = FunctionSignature(name.value, parameters.length)
        generateAggregated(parameters).withPush(Invoke(signature))
    }

  private def generateAggregated(nodes: List[SyntaxTree]): TPLCode =
    nodes
      .map(generate)
      .foldLeft(TPLCode.empty)(_ ++ _)
}