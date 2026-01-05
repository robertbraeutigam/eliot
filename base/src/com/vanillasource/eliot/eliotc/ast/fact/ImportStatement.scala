package com.vanillasource.eliot.eliotc.ast.fact

import cats.Show
import cats.syntax.all.*
import Primitives.*
import com.vanillasource.eliot.eliotc.ast.fact.{ASTComponent, ImportStatement}
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.token.Token
import com.vanillasource.parser.Parser
import com.vanillasource.parser.Parser.*

case class ImportStatement(
    keyword: Sourced[String],
    packageNames: Seq[Sourced[String]],
    moduleName: Sourced[String]
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
    } yield ImportStatement(keyword.map(_.content), packageNames.map(_.map(_.content)), moduleName.map(_.content))

    private def moduleNameOnSameLineAs(keyword: Sourced[Token]) =
      acceptIfAll(isIdentifier, isUpperCase, isOnSameLineAs(keyword))("module name")

    private def packageNameOnSameLineAs(keyword: Sourced[Token]) =
      acceptIfAll(isIdentifier, isLowerCase, isOnSameLineAs(keyword))("package name")

    private def isOnSameLineAs(sample: Sourced[Token])(st: Sourced[Token]) = sample.range.to.line === st.range.from.line
  }
}
