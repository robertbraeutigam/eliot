package com.vanillasource.eliot.eliotc.ast

import cats.Show
import cats.syntax.all.*

case class AST(importStatements: Seq[ImportStatement], functionDefinitions: Seq[FunctionDefinition])

object AST {
  given Show[AST] = (ast: AST) =>
    s"import statements: ${ast.importStatements.map(_.show).mkString(", ")}, function definitions: ${ast.functionDefinitions.map(_.show).mkString(", ")}"
}
