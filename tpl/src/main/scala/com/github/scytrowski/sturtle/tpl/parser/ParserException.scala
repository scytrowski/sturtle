package com.github.scytrowski.sturtle.tpl.parser

final case class ParserException(error: ParseError) extends Exception {
  override def getMessage: String = error.toString
}
