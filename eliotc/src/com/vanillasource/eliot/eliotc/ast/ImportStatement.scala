package com.vanillasource.eliot.eliotc.ast

import cats.Show
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.source.Sourced
import com.vanillasource.eliot.eliotc.token.Token

case class ImportStatement(
    keyword: Sourced[Token],
    packageNames: Seq[Sourced[Token]],
    moduleName: Sourced[Token]
) {
  def outline: Sourced[Unit] = Sourced.outline(packageNames :+ moduleName)
}

object ImportStatement {
  given Show[ImportStatement] = (t: ImportStatement) => s"${(t.packageNames :+ t.moduleName).map(_.show).mkString(".")}"
}
