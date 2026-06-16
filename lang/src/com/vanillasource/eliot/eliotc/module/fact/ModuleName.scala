package com.vanillasource.eliot.eliotc.module.fact

import cats.{Eq, Show}
import com.vanillasource.eliot.eliotc.ast.fact.ImportStatement

case class ModuleName(packages: Seq[String], name: String)

object ModuleName {
  def fromImportStatement(importStatement: ImportStatement): ModuleName =
    ModuleName(importStatement.packageNames.map(_.value), importStatement.moduleName.value)

  given Show[ModuleName] = m => (m.packages :+ m.name).mkString(".")

  given Eq[ModuleName] = Eq.fromUniversalEquals

  // FIXME: look at this, don't throw
  def parse(s: String): ModuleName = s.split('.') match
    case parts if parts.length > 0 => ModuleName(parts.toIndexedSeq.take(parts.length - 1), parts.last)
    case _                         => throw IllegalArgumentException(s"Can not parse '$s' into module name.")

  val defaultSystemPackage = Seq("eliot", "lang")

  val systemFunctionModuleName: ModuleName = ModuleName(defaultSystemPackage, "Function")
  // TODO: Unit is no longer here, so we shouldn't refer to it...
  // This is used to determine what to automatically import, but this should work differently.
  // NOTE: anything added here is auto-imported into every module, so the test harness must provide a matching stub
  // (see ProcessorTest's default SystemImports). `Int`/`Runtime` (integer arithmetic + `integerLiteral`) are therefore
  // NOT auto-imported yet — code that uses them imports `eliot.lang.Int` / `eliot.lang.Runtime` explicitly. Making them
  // ambient is deferred to the Phase-6 literal desugar, which will add them here together with the test stubs. See
  // `docs/int-min-max-plan.md`.
  val defaultSystemModules                 = Seq(
    "Type",
    "Function",
    "Unit",
    "String",
    "BigInteger",
    "IO",
    "PatternMatch",
    "TypeMatch"
  ).map(ModuleName(defaultSystemPackage, _))

}
