package com.github.scytrowski.sturtle.tpl.parser.expression

import com.github.scytrowski.sturtle.tpl.codegen.SyntaxTree.Expression
import com.github.scytrowski.sturtle.tpl.codegen.SyntaxTree.Expression.{FunctionCall, Name, Static}
import com.github.scytrowski.sturtle.tpl.fixture.EffectSpecLike
import com.github.scytrowski.sturtle.tpl.interpreter.{BooleanValue, NumberValue, StringValue}
import com.github.scytrowski.sturtle.tpl.parser.{SyntaxTreeGenerator, Token}
import com.github.scytrowski.sturtle.tpl.types.Complex
import org.scalatest.Inside

class ExpressionParserTest extends EffectSpecLike with SyntaxTreeGenerator with Inside { gen: SyntaxTreeGenerator =>
  "ExpressionParser" should {
    "parse" when {
      "name" in {
        val tokens = List(Token.NameToken("a"))

        parse(tokens) mustBe Name("a")
      }

      "boolean" in {
        val tokens = List(Token.BooleanToken(true))

        parse(tokens) mustBe Static(BooleanValue(true))
      }

      "number" in {
        val tokens = List(Token.NumberToken(Complex.real(1337)))

        parse(tokens) mustBe Static(NumberValue(Complex.real(1337)))
      }

      "string" in {
        val tokens = List(Token.StringToken("abcdefgh123456"))

        parse(tokens) mustBe Static(StringValue("abcdefgh123456"))
      }

      "point" in {
        val tokens = List(
          Token.RoundBracketOpen,
          Token.NumberToken(Complex.real(1.23)),
          Token.Comma,
          Token.NumberToken(Complex.real(4.56)),
          Token.RoundBracketClose
        )

        parse(tokens) mustBe gen.point(
          Static(NumberValue(Complex.real(1.23))),
          Static(NumberValue(Complex.real(4.56)))
        )
      }

      "vector" in {
        val tokens = List(
          Token.SquareBracketOpen,
          Token.NumberToken(Complex.real(9.87)),
          Token.Comma,
          Token.NumberToken(Complex.real(6.54)),
          Token.SquareBracketClose
        )

        parse(tokens) mustBe gen.vector(
          Static(NumberValue(Complex.real(9.87))),
          Static(NumberValue(Complex.real(6.54)))
        )
      }

      "color" in {
        val tokens = List(
          Token.CurlyBracketOpen,
          Token.NameToken("r"),
          Token.Comma,
          Token.NameToken("g"),
          Token.Comma,
          Token.NameToken("b"),
          Token.CurlyBracketClose
        )

        parse(tokens) mustBe gen.color(
          Name("r"),
          Name("g"),
          Name("b")
        )
      }

      "inside round bracket" in {
        val tokens = List(
          Token.RoundBracketOpen,
          Token.NameToken("a"),
          Token.RoundBracketClose
        )

        parse(tokens) mustBe Name("a")
      }

      "with prefix plus" in {
        val tokens = List(
          Token.Plus,
          Token.NameToken("a")
        )

        parse(tokens) mustBe gen.plus(Name("a"))
      }

      "with prefix minus" in {
        val tokens = List(
          Token.Minus,
          Token.NameToken("a")
        )

        parse(tokens) mustBe gen.minus(Name("a"))
      }

      "with addition" in {
        val tokens = List(
          Token.NameToken("a"),
          Token.Plus,
          Token.NameToken("b")
        )

        parse(tokens) mustBe gen.add(Name("a"), Name("b"))
      }

      "with subtraction" in {
        val tokens = List(
          Token.NameToken("a"),
          Token.Minus,
          Token.NameToken("b")
        )

        parse(tokens) mustBe gen.subtract(Name("a"), Name("b"))
      }

      "with multiplication" in {
        val tokens = List(
          Token.NameToken("a"),
          Token.Star,
          Token.NameToken("b")
        )

        parse(tokens) mustBe gen.multi(Name("a"), Name("b"))
      }

      "with division" in {
        val tokens = List(
          Token.NameToken("a"),
          Token.Slash,
          Token.NameToken("b")
        )

        parse(tokens) mustBe gen.div(Name("a"), Name("b"))
      }

      "with assignment" in {
        val tokens = List(
          Token.NameToken("a"),
          Token.EqualsSign,
          Token.NameToken("b")
        )

        parse(tokens) mustBe gen.assignment(Name("a"), Name("b"))
      }

      "with equal to" in {
        val tokens = List(
          Token.NameToken("a"),
          Token.EqualsSign,
          Token.EqualsSign,
          Token.NameToken("b")
        )

        parse(tokens) mustBe gen.equal(Name("a"), Name("b"))
      }

      "with not equal to" in {
        val tokens = List(
          Token.NameToken("a"),
          Token.LessThanSign,
          Token.GreaterThanSign,
          Token.NameToken("b")
        )

        parse(tokens) mustBe gen.notEqual(Name("a"), Name("b"))
      }

      "with less than" in {
        val tokens = List(
          Token.NameToken("a"),
          Token.LessThanSign,
          Token.NameToken("b")
        )

        parse(tokens) mustBe gen.less(Name("a"), Name("b"))
      }

      "with less or equal to" in {
        val tokens = List(
          Token.NameToken("a"),
          Token.LessThanSign,
          Token.EqualsSign,
          Token.NameToken("b")
        )

        parse(tokens) mustBe gen.lessOrEqual(Name("a"), Name("b"))
      }

      "with greater than" in {
        val tokens = List(
          Token.NameToken("a"),
          Token.GreaterThanSign,
          Token.NameToken("b")
        )

        parse(tokens) mustBe gen.greater(Name("a"), Name("b"))
      }

      "with greater or equal to" in {
        val tokens = List(
          Token.NameToken("a"),
          Token.GreaterThanSign,
          Token.EqualsSign,
          Token.NameToken("b")
        )

        parse(tokens) mustBe gen.greaterOrEqual(Name("a"), Name("b"))
      }

      "with negation" in {
        val tokens = List(
          Token.Not,
          Token.NameToken("a")
        )

        parse(tokens) mustBe gen.negate(Name("a"))
      }

      "with logical and" in {
        val tokens = List(
          Token.NameToken("a"),
          Token.And,
          Token.NameToken("b")
        )

        parse(tokens) mustBe gen.and(Name("a"), Name("b"))
      }

      "with logical or" in {
        val tokens = List(
          Token.NameToken("a"),
          Token.Or,
          Token.NameToken("b")
        )

        parse(tokens) mustBe gen.or(Name("a"), Name("b"))
      }

      "function call" in {
        val tokens = List(
          Token.NameToken("f"),
          Token.RoundBracketOpen,
          Token.NameToken("a"),
          Token.Comma,
          Token.NameToken("b"),
          Token.RoundBracketClose
        )

        parse(tokens) mustBe FunctionCall(
          Name("f"),
          List(Name("a"), Name("b"))
        )
      }

      "composite function call" when {
        "composed with another function" in {
          val tokens = List(
            Token.NameToken("f"),
            Token.RoundBracketOpen,
            Token.NameToken("g"),
            Token.RoundBracketOpen,
            Token.NameToken("x"),
            Token.RoundBracketClose,
            Token.RoundBracketClose
          )

          parse(tokens) mustBe FunctionCall(
            Name("f"),
            List(FunctionCall(
              Name("g"),
              List(Name("x"))
            ))
          )
        }

        "composed with operator" in {
          val tokens = List(
            Token.NameToken("f"),
            Token.RoundBracketOpen,
            Token.NameToken("x"),
            Token.Star,
            Token.NameToken("y"),
            Token.RoundBracketClose
          )

          parse(tokens) mustBe FunctionCall(
            Name("f"),
            List(gen.multi(Name("x"), Name("y")))
          )
        }
      }
    }
  }

  private def parse(tokens: List[Token]): Expression =
    inside(ExpressionParser.parse(tokens).toEither) { case Right((result, remaining)) =>
      remaining.isEmpty mustBe true

      result
    }
}
