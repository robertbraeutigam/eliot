package com.vanillasource.eliot.eliotc.ast

import cats.Show
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.ast.Primitives.*
import com.vanillasource.eliot.eliotc.source.Sourced
import com.vanillasource.eliot.eliotc.token.Token
import com.vanillasource.parser.Parser
import com.vanillasource.parser.Parser.*

case class ImportStatement(
    keyword: Sourced[Token],
    packageNames: Seq[Sourced[Token]],
    moduleName: Sourced[Token]
) {
  def outline: Sourced[Unit] = Sourced.outline(packageNames :+ moduleName)
}

object ImportStatement {
  given Show[ImportStatement] = (t: ImportStatement) => s"${(t.packageNames :+ t.moduleName).map(_.show).mkString(".")}"

  given ASTComponent[ImportStatement] = new ASTComponent[ImportStatement] {
    override def parser: Parser[Sourced[Token], ImportStatement] = for {
      keyword      <- topLevelKeyword("import")
      packageNames <- (packageNameOnSameLineAs(keyword) <* symbol(".")).anyTimes()
      moduleName   <- moduleNameOnSameLineAs(keyword)
    } yield ImportStatement(keyword, packageNames, moduleName)

    private def moduleNameOnSameLineAs(keyword: Sourced[Token]) =
      acceptIfAll(isIdentifier, isUpperCase, isOnSameLineAs(keyword))("module name")

    private def packageNameOnSameLineAs(keyword: Sourced[Token]) =
      acceptIfAll(isIdentifier, isLowerCase, isOnSameLineAs(keyword))("package name")

    private def isOnSameLineAs(sample: Sourced[Token])(st: Sourced[Token]) = sample.range.to.line === st.range.from.line
  }
}
