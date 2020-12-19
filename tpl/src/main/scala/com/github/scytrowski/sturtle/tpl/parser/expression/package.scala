package com.github.scytrowski.sturtle.tpl.parser

import com.github.scytrowski.sturtle.tpl.codegen.SyntaxTree.Expression

package object expression {
  type ParseExpressions[+A] = Parse[Expression, ExpressionError, A]
}
