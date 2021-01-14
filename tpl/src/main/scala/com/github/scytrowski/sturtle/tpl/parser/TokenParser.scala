package com.github.scytrowski.sturtle.tpl.parser

import com.github.scytrowski.sturtle.tpl.codegen.SyntaxTree.Expression
import com.github.scytrowski.sturtle.tpl.codegen.SyntaxTree.Expression.{FunctionCall, Name}
import com.github.scytrowski.sturtle.tpl.parser.ParseError.UnexpectedToken
import com.github.scytrowski.sturtle.tpl.parser.expression.ExpressionParser

trait TokenParser[+A] extends Parser[Token, ParseError, A] with TokenParserFactory with SyntaxTreeGenerator {
  protected def parameterList(bracketType: BracketType): P[List[Expression]] = {
    val parameters = unfoldWhileDefinedS(true) {
      case true =>
        peek.flatMap {
          case token if token == bracketType.closeToken => drop(1).as(None)
          case _ =>
            expression.flatMap { param =>
              head.flatMap {
                case Token.Comma => succeed(true -> param).option
                case token if token == bracketType.closeToken => succeed(false -> param).option
                case unexpected => fail(UnexpectedToken(unexpected))
              }
            }
        }
      case false => succeed(Option.empty[(Boolean, Expression)])
    }

    require(bracketType.openToken) *> parameters
  }

  protected def expression: P[Expression] = ExpressionParser
}
