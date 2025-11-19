package com.vanillasource.eliot.eliotc.module.fact

import cats.{Eq, Show}
import com.vanillasource.eliot.eliotc.ast.ImportStatement

case class ModuleName(packages: Seq[String], name: String)

object ModuleName {
  def fromImportStatement(importStatement: ImportStatement): ModuleName =
    ModuleName(importStatement.packageNames.map(_.value), importStatement.moduleName.value)

  given Show[ModuleName] = m => (m.packages :+ m.name).mkString(".")

  given Eq[ModuleName] = Eq.fromUniversalEquals

  def parse(s: String): ModuleName = s.split('.') match
    case parts if parts.length > 0 => ModuleName(parts.take(parts.length - 1), parts.last)
    case _                         => throw IllegalArgumentException(s"Can not parse '$s' into module name.")

  private val defaultSystemPackage = Seq("eliot", "lang")

  val systemFunctionModuleName: ModuleName = ModuleName(defaultSystemPackage, "Function")
  val defaultSystemModules                 = Seq(
    "Function",
    "Unit",
    "String"
  ).map(ModuleName(defaultSystemPackage, _))

}
