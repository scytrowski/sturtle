package com.github.scytrowski.sturtle.tpl.lexer

import cats.MonadError
import com.github.scytrowski.sturtle.tpl.lexer.LexerError.{InvalidInteger, InvalidNumber, InvalidSymbol, UnclosedString}
import com.github.scytrowski.sturtle.tpl.parser.Token
import com.github.scytrowski.sturtle.tpl.types.Complex
import fs2.{Pipe, Stream}

import scala.language.postfixOps

final class TPLLexer[F[_]](implicit monadError: MonadError[F, Throwable]) extends Lexer[F, String, Token] {
  import Token._

  override def pipe: Pipe[F, String, Token] =
    _.through(extractLines)
      .zipWithIndex
      .flatMap(tokenizeLine _ tupled)

  private def extractLines: Pipe[F, String, String] =
    _.flatMap(s => Stream.iterable(s.split('\n')))

  private def tokenizeLine(line: String, lineNumber: Long): Stream[F, Token] =
    if (line.nonEmpty) {
      val continue = tokenizeLine(_: String, lineNumber)
      line.head match {
        case '(' => Stream(RoundBracketOpen) ++ continue(line.tail)
        case ')' => Stream(RoundBracketClose) ++ continue(line.tail)
        case '[' => Stream(SquareBracketOpen) ++ continue(line.tail)
        case ']' => Stream(SquareBracketClose) ++ continue(line.tail)
        case '{' => Stream(CurlyBracketOpen) ++ continue(line.tail)
        case '}' => Stream(CurlyBracketClose) ++ continue(line.tail)
        case '+' => Stream(Plus) ++ continue(line.tail)
        case '-' => Stream(Minus) ++ continue(line.tail)
        case '*' => Stream(Star) ++ continue(line.tail)
        case '/' => Stream(Slash) ++ continue(line.tail)
        case '=' => Stream(EqualsSign) ++ continue(line.tail)
        case '<' => Stream(LessThanSign) ++ continue(line.tail)
        case '>' => Stream(GreaterThanSign) ++ continue(line.tail)
        case ':' => Stream(Colon) ++ continue(line.tail)
        case ',' => Stream(Comma) ++ continue(line.tail)
        case '"' => extractString(line.tail, lineNumber)
        case c if c.isWhitespace => continue(line.dropWhile(_.isWhitespace))
        case c if c.isLetter => extractNameOrKeyword(line, lineNumber)
        case c if c.isDigit => extractNumber(line, lineNumber)
        case invalid => raiseError(InvalidSymbol(lineNumber, invalid))
      }
    } else
      Stream(EOL)

  private def extractString(line: String, lineNumber: Long): Stream[F, Token] = {
    val endIndex = line.indexOf('"')
    if (endIndex >= 0) {
      val value = line.take(endIndex)
      Stream(StringToken(value)) ++ tokenizeLine(line.drop(endIndex + 1), lineNumber)
    } else
      raiseError(UnclosedString(lineNumber))
  }

  private def extractNameOrKeyword(line: String, lineNumber: Long): Stream[F, Token] = {
    val name = line.takeWhile(_.isLetterOrDigit)
    val token = name match {
      case "true"  => BooleanToken(true)
      case "false" => BooleanToken(false)
      case "block" => Block
      case "function" => Function
      case "if" => If
      case "elif" => Elif
      case "else" => Else
      case "then" => Then
      case "while" => While
      case "do" => Do
      case "break" => Break
      case "return" => Return
      case "end" => End
      case "not" => Not
      case "and" => And
      case "or" => Or
      case other => NameToken(other)
    }
    Stream(token) ++ tokenizeLine(line.drop(name.length), lineNumber)
  }

  private def extractNumber(line: String, lineNumber: Long): Stream[F, Token] = {
    val (value, r1) = extractNumberValue(line)
    if (r1.startsWith("e") || r1.startsWith("E"))
      extractInteger(r1.tail, lineNumber).flatMap { case (expValue, r2) =>
        parseComplex(s"${value}E$expValue", lineNumber, r2)
      }
    else
      parseComplex(value, lineNumber, r1)
  }

  private def extractInteger(line: String, lineNumber: Long): Stream[F, (String, String)] = {
    val (sign, l) = if (line.startsWith("-")) "-" -> line.tail else "" -> line
    val (v, r) = extractNumberValue(l)
    if (!v.contains('.'))
      Stream(s"$sign$v" -> r)
    else
      raiseError(InvalidInteger(lineNumber))
  }

  private def extractNumberValue(line: String): (String, String) = {
    val digits = line.takeWhile(_.isDigit)
    val rest = line.drop(digits.length)
    val number =
      if (rest.startsWith(".")) {
        val decimalDigits = rest.tail.takeWhile(_.isDigit)
        s"$digits.$decimalDigits"
      } else digits
    number -> line.drop(number.length)
  }

  private def parseComplex(str: String, lineNumber: Long, remaining: String): Stream[F, Token] =
    parseNumber(str, lineNumber).flatMap { n =>
      if (remaining.startsWith("i"))
        Stream(NumberToken(Complex.imaginary(n))) ++ tokenizeLine(remaining.tail, lineNumber)
      else
        Stream(NumberToken(Complex.real(n))) ++ tokenizeLine(remaining, lineNumber)
    }

  private def parseNumber(str: String, lineNumber: Long): Stream[F, Double] =
    str.toDoubleOption match {
      case Some(value) => Stream(value)
      case None        => raiseError(InvalidNumber(lineNumber))
    }

  private def raiseError(error: LexerError): Stream[F, Nothing] = Stream.raiseError(LexerException(error))
}
