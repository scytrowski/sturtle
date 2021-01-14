package com.github.scytrowski.sturtle.tpl.lexer

import cats.effect.IO
import com.github.scytrowski.sturtle.tpl.fixture.{EffectSpecLike, RandomnessFixture}
import com.github.scytrowski.sturtle.tpl.lexer.LexerError.{InvalidInteger, InvalidSymbol, UnclosedString}
import com.github.scytrowski.sturtle.tpl.parser.Token
import com.github.scytrowski.sturtle.tpl.types.Complex
import fs2.Stream
import org.scalatest.prop.TableDrivenPropertyChecks._
import org.scalatest.{Inside, OptionValues, TryValues}

import scala.util.{Failure, Success, Try}

class TPLLexerTest extends EffectSpecLike with RandomnessFixture with TryValues with OptionValues with Inside {
  import Token._

  "TPLLexer" should {
    "emit RoundBracketOpen" in {
      requireTokenized("(") { case RoundBracketOpen :: Nil => }
    }

    "emit RoundBracketClose" in {
      requireTokenized(")") { case RoundBracketClose :: Nil => }
    }

    "emit SquareBracketOpen" in {
      requireTokenized("[") { case SquareBracketOpen :: Nil => }
    }

    "emit SquareBracketClose" in {
      requireTokenized("]") { case SquareBracketClose :: Nil => }
    }

    "emit CurlyBracketOpen" in {
      requireTokenized("{") { case CurlyBracketOpen :: Nil => }
    }

    "emit CurlyBracketClose" in {
      requireTokenized("}") { case CurlyBracketClose :: Nil => }
    }

    "emit Plus" in {
      requireTokenized("+") { case Plus :: Nil => }
    }

    "emit Minus" in {
      requireTokenized("-") { case Minus :: Nil => }
    }

    "emit Star" in {
      requireTokenized("*") { case Star :: Nil => }
    }

    "emit Slash" in {
      requireTokenized("/") { case Slash :: Nil => }
    }

    "emit EqualsSign" in {
      requireTokenized("=") { case EqualsSign :: Nil => }
    }

    "emit LessThanSign" in {
      requireTokenized("<") { case LessThanSign :: Nil => }
    }

    "emit GreaterThanSign" in {
      requireTokenized(">") { case GreaterThanSign :: Nil => }
    }

    "emit Colon" in {
      requireTokenized(":") { case Colon :: Nil => }
    }

    "emit Comma" in {
      requireTokenized(",") { case Comma :: Nil => }
    }

    "emit true" in {
      requireTokenized("true") { case BooleanToken(true) :: Nil => }
    }

    "emit false" in {
      requireTokenized("false") { case BooleanToken(false) :: Nil => }
    }

    "emit Block" in {
      requireTokenized("block") { case Block :: Nil => }
    }

    "emit Function" in {
      requireTokenized("function") { case Function :: Nil => }
    }

    "emit If" in {
      requireTokenized("if") { case If :: Nil => }
    }

    "emit Elif" in {
      requireTokenized("elif") { case Elif :: Nil => }
    }

    "emit Else" in {
      requireTokenized("else") { case Else :: Nil => }
    }

    "emit Then" in {
      requireTokenized("then") { case Then :: Nil => }
    }

    "emit While" in {
      requireTokenized("while") { case While :: Nil => }
    }

    "emit Do" in {
      requireTokenized("do") { case Do :: Nil => }
    }

    "emit Break" in {
      requireTokenized("break") { case Break :: Nil => }
    }

    "emit Return" in {
      requireTokenized("return") { case Return :: Nil => }
    }

    "emit End" in {
      requireTokenized("end") { case End :: Nil => }
    }

    "emit Not" in {
      requireTokenized("not") { case Not :: Nil => }
    }

    "emit And" in {
      requireTokenized("and") { case And :: Nil => }
    }

    "emit Or" in {
      requireTokenized("or") { case Or :: Nil => }
    }

    "emit NameToken" in {
      forAll(Table("n", randomElements[NameToken](1000):_*)) { t =>
        requireTokenized(t.value) { case (t2: NameToken) :: Nil if t == t2 => }
      }
    }

    "emit StringToken" in {
      forAll(Table("s", randomElements[String](1000):_*)) { s =>
        requireTokenized(List("\"", s, "\"").mkString) { case StringToken(v) :: Nil if v == s => }
      }
    }

    "emit NumberToken" when {
      "number is an integer" in {
        forAll(Table("n", randomElements[Int](1000):_*)) { n =>
          requireTokenized(n.toString) { case NumberToken(v) :: Nil if v == n => }
        }
      }

      "number has fractional digits" in {
        forAll(Table("q", randomElements[Double](1000, _ >= 0):_*)) { q =>
          requireTokenized(q.toString) { case NumberToken(v) :: Nil if v == q => }
        }
      }

      "number has preceding plus" in {
        forAll(Table("q", randomElements[Int](1000):_*)) { q =>
          requireTokenized(s"+$q") { case Plus :: NumberToken(v) :: Nil if v == q => }
        }
      }

      "number has preceding minus" in {
        forAll(Table("q", randomElements[Int](1000):_*)) { q =>
          requireTokenized(s"-$q") { case Minus :: NumberToken(v) :: Nil if v == q => }
        }
      }

      "number is imaginary" in {
        forAll(Table("q", randomElements[Double](1000, _ >= 0):_*)) { q =>
          requireTokenized(s"${q}i") { case NumberToken(v) :: Nil if v == Complex.imaginary(q) => }
        }
      }

      "number is complex" in {
        forAll(Table(("re", "im"), randomPairs[Double](1000, _ >= 0):_*)) { case (re, im) =>
          requireTokenized(s"$re+${im}i") { case NumberToken(r) :: Plus :: NumberToken(i) :: Nil =>
            r mustBe Complex.real(re)
            i mustBe Complex.imaginary(im)
          }
        }
      }

      "number is conjugate of complex number" in {
        forAll(Table(("re", "im"), randomPairs[Double](1000, _ >= 0):_*)) { case (re, im) =>
          requireTokenized(s"$re-${im}i") { case NumberToken(r) :: Minus :: NumberToken(i) :: Nil =>
            r mustBe Complex.real(re)
            i mustBe Complex.imaginary(im)
          }
        }
      }
    }

    "emit block" in {
      val source =
        """block
          |   break
          |end
          |""".stripMargin

      val tokens = tokenize(source).success.value

      tokens mustBe List(
        Block,
        EOL,
        Break,
        EOL,
        End,
        EOL
      )
    }

    "emit function definition" when {
      "single line" in {
        val tokens = tokenize("function f(x) return x end").success.value

        tokens mustBe List(
          Function,
          NameToken("f"),
          RoundBracketOpen,
          NameToken("x"),
          RoundBracketClose,
          Return,
          NameToken("x"),
          End,
          EOL
        )
      }

      "multi line" in {
        val source =
          """function f(x)
            |   return x
            |end""".stripMargin

        val tokens = tokenize(source).success.value

        tokens mustBe List(
          Function,
          NameToken("f"),
          RoundBracketOpen,
          NameToken("x"),
          RoundBracketClose,
          EOL,
          Return,
          NameToken("x"),
          EOL,
          End,
          EOL
        )
      }
    }

    "emit if block" in {
      val source = "if n = 5 then break end"

      val tokens = tokenize(source).success.value

      tokens mustBe List(
        If,
        NameToken("n"),
        EqualsSign,
        NumberToken(Complex.real(5)),
        Then,
        Break,
        End,
        EOL
      )
      requireTokenized(source) { case If :: NameToken("n") :: EqualsSign :: NumberToken(Complex(5, 0)) :: Then :: Break :: End :: Nil => }
    }

    "skip initial whitespaces" in {
      requireTokenized(" \t =") { case EqualsSign :: Nil => }
    }

    "skip trailing whitespaces" in {
      requireTokenized("/    \t\t\t ") { case Slash :: Nil => }
    }

    "fail" when {
      "head symbol is invalid" in {
        val invalidSymbols = Table("s",
          '!', '@', '#', '$', '%', '^', '~',
          '_', '&', ';', '?', '\\', '.', '\''
        )
        forAll(invalidSymbols) { s =>
          requireFailed(s.toString) mustBe InvalidSymbol(0, s)
        }
      }

      "unclosed string" in {
        requireFailed("\"abc") mustBe UnclosedString(0)
      }

      "invalid integer as an exponent in the scientific notation" in {
        requireFailed("1E1.521") mustBe InvalidInteger(0)
      }
    }
  }

  private def requireTokenized[U](source: String)(pf: PartialFunction[List[Token], U]): U =
    inside(tokenize(source)) { case Success(tokens) =>
      val last = tokens.lastOption.value
      last mustBe EOL

      inside(tokens.dropRight(1))(pf)
    }

  private def requireFailed(source: String): LexerError =
    inside(tokenize(source)) { case Failure(LexerException(error)) => error }

  private def tokenize(source: String): Try[List[Token]] = {
    Stream[IO, String](source)
      .through(new TPLLexer[IO].pipe)
      .compile
      .toList
      .attempt
      .runTimed()
      .toTry
  }
}
