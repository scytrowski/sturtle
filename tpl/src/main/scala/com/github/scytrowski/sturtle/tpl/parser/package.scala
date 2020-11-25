package com.github.scytrowski.sturtle.tpl

package object parser {
  type Parse[T, +A] = List[T] => ParseResult[T, A]
}
