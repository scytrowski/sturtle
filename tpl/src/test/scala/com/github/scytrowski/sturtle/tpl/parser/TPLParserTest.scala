package com.github.scytrowski.sturtle.tpl.parser

import cats.data.NonEmptyList
import com.github.scytrowski.sturtle.tpl.codegen.Case.Conditional
import com.github.scytrowski.sturtle.tpl.codegen.SyntaxTree.Expression.{Assignment, Name, Static}
import com.github.scytrowski.sturtle.tpl.codegen.SyntaxTree._
import com.github.scytrowski.sturtle.tpl.codegen.{Case, SyntaxTree}
import com.github.scytrowski.sturtle.tpl.fixture.EffectSpecLike
import com.github.scytrowski.sturtle.tpl.interpreter.VoidValue
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
        NonEmptyList.of(Conditional(
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
          Conditional(
            Name("a"),
            Block(List(Return(Name("b"))))
          ),
          Conditional(
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
          Conditional(
            Name("a"),
            Block(List(Return(Name("b"))))
          ),
          Case.Default(
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
          Conditional(
            Name("a"),
            Block(List(Return(Name("b"))))
          ),
          Conditional(
            Name("b"),
            Block(List(Return(Name("a"))))
          ),
          Case.Default(
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
          Conditional(Name("a"), Block(List(Return(Static(VoidValue))))),
          Conditional(Name("b"), Block(List(Break)))
        ))
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
          Conditional(Name("a"), Block(List(Return(Static(VoidValue))))),
          Case.Default(Block(List(Break)))
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
          Conditional(Name("a"), Block(List(Return(Static(VoidValue)))))
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

    "parse assignment" in {
      val tokens = List(
        Token.NameToken("a"),
        Token.EqualsSign,
        Token.NameToken("b")
      )

      parseSingleStatement(tokens) mustBe Assignment(
        Name("a"),
        Name("b")
      )
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
      }
    }
  }

  private def parseSingleStatement(tokens: List[Token]): SyntaxTree =
    inside(parse(tokens)) { case Block(st :: Nil) => st }

  private def parse(tokens: List[Token]): SyntaxTree =
    inside(TPLParser.parse(tokens).toEither) { case Right((result, remaining)) =>
      remaining.isEmpty mustBe true

      result
    }

  private def expectFailure(tokens: List[Token]): ParseError =
    inside(TPLParser.parse(tokens).toEither) { case Left(error) => error }
}
