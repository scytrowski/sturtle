package com.github.scytrowski.sturtle.tpl.parser

sealed abstract class CaseType

object CaseType {
  case object Conditional extends CaseType
  case object Default extends CaseType
}