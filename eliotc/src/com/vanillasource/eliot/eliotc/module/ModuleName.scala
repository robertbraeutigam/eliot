package com.vanillasource.eliot.eliotc.module

import cats.Show
import com.vanillasource.eliot.eliotc.ast.ImportStatement

case class ModuleName(packages: Seq[String], name: String)

object ModuleName {
  def fromImportStatement(importStatement: ImportStatement): ModuleName =
    ModuleName(importStatement.packageNames.map(_.value.content), importStatement.moduleName.value.content)

  given Show[ModuleName] = m => (m.packages :+ m.name).mkString(".")
}
