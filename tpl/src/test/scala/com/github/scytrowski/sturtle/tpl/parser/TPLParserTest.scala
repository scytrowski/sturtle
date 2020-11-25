package com.github.scytrowski.sturtle.tpl.parser

import cats.data.NonEmptyList
import com.github.scytrowski.sturtle.tpl.fixture.EffectSpecLike
import com.github.scytrowski.sturtle.tpl.interpreter.Case.{ConditionalCase, DefaultCase}
import com.github.scytrowski.sturtle.tpl.interpreter.SyntaxTree
import com.github.scytrowski.sturtle.tpl.interpreter.SyntaxTree.Expression.{FunctionCall, Name, Static}
import com.github.scytrowski.sturtle.tpl.interpreter.SyntaxTree.{Assignment, Block, Branch, Break, Expression, FunctionDefinition, Loop, Return}
import com.github.scytrowski.sturtle.tpl.interpreter.Value.{BooleanValue, NumberValue, StringValue, VoidValue}
import com.github.scytrowski.sturtle.tpl.parser.ParseError.{UnexpectedEndOfStream, UnexpectedToken}
import org.scalatest.Inside

class TPLParserTest extends EffectSpecLike with SyntaxTreeGenerator with Inside { gen: SyntaxTreeGenerator =>
  "TPLParser" should {
    "parse Block" in {
      val tokens = List(Token.Block, Token.Break, Token.End)

      parseSingleStatement(tokens) mustBe Block(List(Break))
    }

    "parse function definition" in {
      val tokens = List(
        Token.Function,
        Token.NameToken("f"),
        Token.RoundBracketOpen,
        Token.NameToken("a"),
        Token.Comma,
        Token.NameToken("b"),
        Token.RoundBracketClose,
        Token.Return,
        Token.NameToken("b"),
        Token.End
      )

      parseSingleStatement(tokens) mustBe FunctionDefinition(
        Name("f"),
        List(Name("a"), Name("b")),
        Block(List(Return(Name("b"))))
      )
    }

    "parse if block" in {
      val tokens = List(
        Token.If,
        Token.NameToken("a"),
        Token.Then,
        Token.Return,
        Token.NameToken("b"),
        Token.End
      )

      parseSingleStatement(tokens) mustBe Branch(
        NonEmptyList.of(ConditionalCase(
          Name("a"),
          Block(List(Return(Name("b"))))
        ))
      )
    }

    "parse if-elif block" in {
      val tokens = List(
        Token.If,
        Token.NameToken("a"),
        Token.Then,
        Token.Return,
        Token.NameToken("b"),
        Token.Elif,
        Token.NameToken("b"),
        Token.Then,
        Token.Return,
        Token.NameToken("a"),
        Token.End
      )

      parseSingleStatement(tokens) mustBe Branch(
        NonEmptyList.of(
          ConditionalCase(
            Name("a"),
            Block(List(Return(Name("b"))))
          ),
          ConditionalCase(
            Name("b"),
            Block(List(Return(Name("a"))))
          )
        )
      )
    }

    "parse if-else block" in {
      val tokens = List(
        Token.If,
        Token.NameToken("a"),
        Token.Then,
        Token.Return,
        Token.NameToken("b"),
        Token.Else,
        Token.Return,
        Token.NameToken("a"),
        Token.End
      )

      parseSingleStatement(tokens) mustBe Branch(
        NonEmptyList.of(
          ConditionalCase(
            Name("a"),
            Block(List(Return(Name("b"))))
          ),
          DefaultCase(
            Block(List(Return(Name("a"))))
          )
        )
      )
    }

    "parse if-elif-else block" in {
      val tokens = List(
        Token.If,
        Token.NameToken("a"),
        Token.Then,
        Token.Return,
        Token.NameToken("b"),
        Token.Elif,
        Token.NameToken("b"),
        Token.Then,
        Token.Return,
        Token.NameToken("a"),
        Token.Else,
        Token.Return,
        Token.NameToken("c"),
        Token.End
      )

      parseSingleStatement(tokens) mustBe Branch(
        NonEmptyList.of(
          ConditionalCase(
            Name("a"),
            Block(List(Return(Name("b"))))
          ),
          ConditionalCase(
            Name("b"),
            Block(List(Return(Name("a"))))
          ),
          DefaultCase(
            Block(List(Return(Name("c"))))
          )
        )
      )
    }

    "parse while" in {
      val tokens = List(
        Token.While,
        Token.NameToken("a"),
        Token.Do,
        Token.Break,
        Token.End
      )

      parseSingleStatement(tokens) mustBe Loop(
        Name("a"),
        Block(List(Break))
      )
    }

    "parse repeat" in {
      val tokens = List(
        Token.Repeat,
        Token.NameToken("a"),
        Token.Do,
        Token.Break,
        Token.End
      )

      inside(parseSingleStatement(tokens)) { case Block((counterAssignment: Assignment) :: (loop: Loop) :: Nil) =>
        val counterName = counterAssignment.variableName

        counterAssignment.value mustBe Static(NumberValue(0))
        loop.condition mustBe FunctionCall(SpecialNames.lessOrEqual, List(counterName, Name("a")))
        loop.body mustBe Block(List(
          FunctionCall(SpecialNames.add, List(counterName, Static(NumberValue(1)))),
          Break
        ))
      }
    }

    "parse break" in {
      val tokens = List(Token.Break)

      parseSingleStatement(tokens) mustBe Break
    }

    "parse return" when {
      "next token is elif" in {
        val tokens = List(
          Token.If,
          Token.NameToken("a"),
          Token.Then,
          Token.Return,
          Token.Elif,
          Token.NameToken("b"),
          Token.Then,
          Token.Break,
          Token.End
        )

        parseSingleStatement(tokens) mustBe Branch(NonEmptyList.of(
          ConditionalCase(Name("a"), Block(List(Return(Static(VoidValue))))),
          ConditionalCase(Name("b"), Block(List(Break))
        )))
      }

      "next token is else" in {
        val tokens = List(
          Token.If,
          Token.NameToken("a"),
          Token.Then,
          Token.Return,
          Token.Else,
          Token.Break,
          Token.End
        )

        parseSingleStatement(tokens) mustBe Branch(NonEmptyList.of(
          ConditionalCase(Name("a"), Block(List(Return(Static(VoidValue))))),
          DefaultCase(Block(List(Break)))
        ))
      }

      "next token is end" in {
        val tokens = List(
          Token.If,
          Token.NameToken("a"),
          Token.Then,
          Token.Return,
          Token.End,
        )

        parseSingleStatement(tokens) mustBe Branch(NonEmptyList.of(
          ConditionalCase(Name("a"), Block(List(Return(Static(VoidValue)))))
        ))
      }

      "next token is EOL" in {
        val tokens = List(
          Token.Return,
          Token.EOL
        )

        parseSingleStatement(tokens) mustBe Return(Static(VoidValue))
      }

      "next tokens create an expression" in {
        val tokens = List(
          Token.Return,
          Token.NameToken("a")
        )

        parseSingleStatement(tokens) mustBe Return(Name("a"))
      }
    }

    "parse function call" in {
      val tokens = List(
        Token.NameToken("f"),
        Token.RoundBracketOpen,
        Token.NameToken("a"),
        Token.Comma,
        Token.NameToken("b"),
        Token.RoundBracketClose
      )

      parseSingleStatement(tokens) mustBe FunctionCall(
        Name("f"),
        List(Name("a"), Name("b"))
      )
    }

    "parse assignment" in {
      val tokens = List(
        Token.NameToken("a"),
        Token.Colon,
        Token.EqualsSign,
        Token.NameToken("b")
      )

      parseSingleStatement(tokens) mustBe Assignment(
        Name("a"),
        Name("b")
      )
    }

    "parse expression" when {
      "name" in {
        val tokens = List(Token.NameToken("a"))

        parseExpression(tokens) mustBe Name("a")
      }

      "boolean" in {
        val tokens = List(Token.BooleanToken(true))

        parseExpression(tokens) mustBe Static(BooleanValue(true))
      }

      "number" in {
        val tokens = List(Token.NumberToken(1337))

        parseExpression(tokens) mustBe Static(NumberValue(1337))
      }

      "string" in {
        val tokens = List(Token.StringToken("abcdefgh123456"))

        parseExpression(tokens) mustBe Static(StringValue("abcdefgh123456"))
      }

      "point" in {
        val tokens = List(
          Token.RoundBracketOpen,
          Token.NumberToken(1.23),
          Token.Comma,
          Token.NumberToken(4.56),
          Token.RoundBracketClose
        )

        parseExpression(tokens) mustBe gen.point(
          Static(NumberValue(1.23)),
          Static(NumberValue(4.56))
        )
      }

      "vector" in {
        val tokens = List(
          Token.SquareBracketOpen,
          Token.NumberToken(9.87),
          Token.Comma,
          Token.NumberToken(6.54),
          Token.SquareBracketClose
        )

        parseExpression(tokens) mustBe gen.vector(
          Static(NumberValue(9.87)),
          Static(NumberValue(6.54))
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

        parseExpression(tokens) mustBe gen.color(
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

        parseExpression(tokens) mustBe Name("a")
      }

      "with prefix plus" in {
        val tokens = List(
          Token.Plus,
          Token.NameToken("a")
        )

        parseExpression(tokens) mustBe gen.plus(Name("a"))
      }

      "with prefix minus" in {
        val tokens = List(
          Token.Minus,
          Token.NameToken("a")
        )

        parseExpression(tokens) mustBe gen.minus(Name("a"))
      }

      "with addition" in {
        val tokens = List(
          Token.NameToken("a"),
          Token.Plus,
          Token.NameToken("b")
        )

        parseExpression(tokens) mustBe gen.add(Name("a"), Name("b"))
      }

      "with subtraction" in {
        val tokens = List(
          Token.NameToken("a"),
          Token.Minus,
          Token.NameToken("b")
        )

        parseExpression(tokens) mustBe gen.subtract(Name("a"), Name("b"))
      }

      "with multiplication" in {
        val tokens = List(
          Token.NameToken("a"),
          Token.Star,
          Token.NameToken("b")
        )

        parseExpression(tokens) mustBe gen.multi(Name("a"), Name("b"))
      }

      "with division" in {
        val tokens = List(
          Token.NameToken("a"),
          Token.Slash,
          Token.NameToken("b")
        )

        parseExpression(tokens) mustBe gen.div(Name("a"), Name("b"))
      }

      "with equal to" in {
        val tokens = List(
          Token.NameToken("a"),
          Token.EqualsSign,
          Token.NameToken("b")
        )

        parseExpression(tokens) mustBe gen.equal(Name("a"), Name("b"))
      }

      "with not equal to" in {
        val tokens = List(
          Token.NameToken("a"),
          Token.LessThanSign,
          Token.GreaterThanSign,
          Token.NameToken("b")
        )

        parseExpression(tokens) mustBe gen.notEqual(Name("a"), Name("b"))
      }

      "with less than" in {
        val tokens = List(
          Token.NameToken("a"),
          Token.LessThanSign,
          Token.NameToken("b")
        )

        parseExpression(tokens) mustBe gen.less(Name("a"), Name("b"))
      }

      "with less or equal to" in {
        val tokens = List(
          Token.NameToken("a"),
          Token.LessThanSign,
          Token.EqualsSign,
          Token.NameToken("b")
        )

        parseExpression(tokens) mustBe gen.lessOrEqual(Name("a"), Name("b"))
      }

      "with greater than" in {
        val tokens = List(
          Token.NameToken("a"),
          Token.GreaterThanSign,
          Token.NameToken("b")
        )

        parseExpression(tokens) mustBe gen.greater(Name("a"), Name("b"))
      }

      "with greater or equal to" in {
        val tokens = List(
          Token.NameToken("a"),
          Token.GreaterThanSign,
          Token.EqualsSign,
          Token.NameToken("b")
        )

        parseExpression(tokens) mustBe gen.greaterOrEqual(Name("a"), Name("b"))
      }

      "with negation" in {
        val tokens = List(
          Token.Not,
          Token.NameToken("a")
        )

        parseExpression(tokens) mustBe gen.negate(Name("a"))
      }

      "with logical and" in {
        val tokens = List(
          Token.NameToken("a"),
          Token.And,
          Token.NameToken("b")
        )

        parseExpression(tokens) mustBe gen.and(Name("a"), Name("b"))
      }

      "with logical or" in {
        val tokens = List(
          Token.NameToken("a"),
          Token.Or,
          Token.NameToken("b")
        )

        parseExpression(tokens) mustBe gen.or(Name("a"), Name("b"))
      }
    }

    "skip EOLs" in {
      val tokens = List(
        Token.EOL,
        Token.EOL,
        Token.EOL,
        Token.Break
      )

      parseSingleStatement(tokens) mustBe Break
    }

    "fail" when {
      "unexpected token" in {
        val token = Token.StringToken("a")

        expectFailure(List(token)) mustBe UnexpectedToken(token)
      }

      "unexpected end of stream" when {
        "block" in {
          val tokens = List(Token.Block)

          expectFailure(tokens) mustBe UnexpectedEndOfStream
        }

        "if block" in {
          val tokens = List(
            Token.If,
            Token.NameToken("a")
          )

          expectFailure(tokens) mustBe UnexpectedEndOfStream
        }

        "if-elif block" in {
          val tokens = List(
            Token.If,
            Token.NameToken("a"),
            Token.Then,
            Token.Break,
            Token.Elif,
            Token.NameToken("b"),
          )

          expectFailure(tokens) mustBe UnexpectedEndOfStream
        }

        "if-else block" in {
          val tokens = List(
            Token.If,
            Token.NameToken("a"),
            Token.Then,
            Token.Break,
            Token.Else
          )

          expectFailure(tokens) mustBe UnexpectedEndOfStream
        }

        "if-elif-else block" in {
          val tokens = List(
            Token.If,
            Token.NameToken("a"),
            Token.Then,
            Token.Break,
            Token.Elif,
            Token.NameToken("b"),
            Token.Then,
            Token.Break,
            Token.Else
          )

          expectFailure(tokens) mustBe UnexpectedEndOfStream
        }

        "while block" in {
          val tokens = List(
            Token.While,
            Token.NameToken("a")
          )

          expectFailure(tokens) mustBe UnexpectedEndOfStream
        }

        "repeat block" in {
          val tokens = List(
            Token.Repeat,
            Token.NameToken("a")
          )

          expectFailure(tokens) mustBe UnexpectedEndOfStream
        }

        "name token" in {
          val tokens = List(Token.NameToken("a"))

          expectFailure(tokens) mustBe UnexpectedEndOfStream
        }
      }
    }
  }

  private def parseExpression(tokens: List[Token]): Expression =
    inside(parseSingleStatement(Token.Return +: tokens)) { case Return(expr) => expr }

  private def parseSingleStatement(tokens: List[Token]): SyntaxTree =
    inside(parse(tokens)) { case Block(st :: Nil) => st }

  private def parse(tokens: List[Token]): SyntaxTree =
    inside(TPLParser.parse(tokens).toEither) { case Right((result, remaining)) =>
      remaining.isEmpty mustBe true

      result
    }

  private def expectFailure(tokens: List[Token]): ParseError[Token] =
    inside(TPLParser.parse(tokens).toEither) { case Left(error) => error }
}
