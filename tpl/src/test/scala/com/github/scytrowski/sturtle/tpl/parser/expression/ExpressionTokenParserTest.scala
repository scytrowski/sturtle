package com.github.scytrowski.sturtle.tpl.parser.expression

import com.github.scytrowski.sturtle.tpl.codegen.SyntaxTree.Expression
import com.github.scytrowski.sturtle.tpl.codegen.SyntaxTree.Expression.Name
import com.github.scytrowski.sturtle.tpl.fixture.CommonSpecLike
import com.github.scytrowski.sturtle.tpl.parser.expression.ExpressionToken.{OperatorToken, ValueToken}
import com.github.scytrowski.sturtle.tpl.parser.{ParseResult, SyntaxTreeGenerator}
import org.scalatest.Inside

class ExpressionTokenParserTest extends CommonSpecLike with SyntaxTreeGenerator with Inside { gen: SyntaxTreeGenerator =>
  "ExpressionTokenParser" should {
    "parse equal" in {
      val tokens = List(
        ValueToken(Name("a")),
        ValueToken(Name("b")),
        OperatorToken(Operator.Equal)
      )

      parse(tokens) mustBe gen.equal(Name("a"), Name("b"))
    }

    "parse not equal" in {
      val tokens = List(
        ValueToken(Name("a")),
        ValueToken(Name("b")),
        OperatorToken(Operator.NotEqual)
      )

      parse(tokens) mustBe gen.notEqual(Name("a"), Name("b"))
    }

    "parse less" in {
      val tokens = List(
        ValueToken(Name("a")),
        ValueToken(Name("b")),
        OperatorToken(Operator.Less)
      )

      parse(tokens) mustBe gen.less(Name("a"), Name("b"))
    }

    "parse less or equal" in {
      val tokens = List(
        ValueToken(Name("a")),
        ValueToken(Name("b")),
        OperatorToken(Operator.LessOrEqual)
      )

      parse(tokens) mustBe gen.lessOrEqual(Name("a"), Name("b"))
    }

    "parse greater" in {
      val tokens = List(
        ValueToken(Name("a")),
        ValueToken(Name("b")),
        OperatorToken(Operator.Greater)
      )

      parse(tokens) mustBe gen.greater(Name("a"), Name("b"))
    }

    "parse greater or equal" in {
      val tokens = List(
        ValueToken(Name("a")),
        ValueToken(Name("b")),
        OperatorToken(Operator.GreaterOrEqual)
      )

      parse(tokens) mustBe gen.greaterOrEqual(Name("a"), Name("b"))
    }

    "parse negate" in {
      val tokens = List(
        ValueToken(Name("a")),
        OperatorToken(Operator.Negate)
      )

      parse(tokens) mustBe gen.negate(Name("a"))
    }

    "parse and" in {
      val tokens = List(
        ValueToken(Name("a")),
        ValueToken(Name("b")),
        OperatorToken(Operator.And)
      )

      parse(tokens) mustBe gen.and(Name("a"), Name("b"))
    }

    "parse or" in {
      val tokens = List(
        ValueToken(Name("a")),
        ValueToken(Name("b")),
        OperatorToken(Operator.Or)
      )

      parse(tokens) mustBe gen.or(Name("a"), Name("b"))
    }

    "parse plus" in {
      val tokens = List(
        ValueToken(Name("a")),
        OperatorToken(Operator.Plus)
      )

      parse(tokens) mustBe gen.plus(Name("a"))
    }

    "parse addition" in {
      val tokens = List(
        ValueToken(Name("a")),
        ValueToken(Name("b")),
        OperatorToken(Operator.Add)
      )

      parse(tokens) mustBe gen.add(Name("a"), Name("b"))
    }

    "parse minus" in {
      val tokens = List(
        ValueToken(Name("a")),
        OperatorToken(Operator.Minus)
      )

      parse(tokens) mustBe gen.minus(Name("a"))
    }

    "parse subtraction" in {
      val tokens = List(
        ValueToken(Name("a")),
        ValueToken(Name("b")),
        OperatorToken(Operator.Subtract)
      )

      parse(tokens) mustBe gen.subtract(Name("a"), Name("b"))
    }

    "parse multiplication" in {
      val tokens = List(
        ValueToken(Name("a")),
        ValueToken(Name("b")),
        OperatorToken(Operator.Multiply)
      )

      parse(tokens) mustBe gen.multi(Name("a"), Name("b"))
    }

    "parse division" in {
      val tokens = List(
        ValueToken(Name("a")),
        ValueToken(Name("b")),
        OperatorToken(Operator.Divide)
      )

      parse(tokens) mustBe gen.div(Name("a"), Name("b"))
    }
  }

  private def parse(tokens: List[ExpressionToken]): Expression =
    inside(ExpressionTokenParser.parse(tokens)) { case ParseResult.Success(expr, remaining) =>
      remaining.isEmpty mustBe true
      expr
    }
}

