package com.github.scytrowski.sturtle.tpl

package object parser {
  type ParseTokens[+A] = Parse[Token, ParseError, A]

  type Parse[T, +E, +A] = List[T] => ParseResult[T, E, A]
}
