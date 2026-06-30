package com.vanillasource.eliot.eliotc.module.fact

import cats.{Eq, Show}
import com.vanillasource.eliot.eliotc.ast.fact.ImportStatement

import java.nio.file.{Path, Paths}

case class ModuleName(packages: Seq[String], name: String) {

  /** The relative `.els` source path this module resolves to under any layer's `eliot/` source root, e.g.
    * `eliot.lang.String` -> `eliot/lang/String.els`. This is the single definition of the package-to-path layout,
    * shared by the source resolver ([[com.vanillasource.eliot.eliotc.module.processor.UnifiedModuleNamesProcessor]],
    * `UnifiedModuleValueProcessor`) and the test harness, so relocating a module to a new package needs no path string
    * updated anywhere.
    */
  def toPath: Path = (packages :+ s"$name.els").foldLeft(Paths.get(""))(_ `resolve` _)
}

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

  /** The package for the effect surface and its machinery: the abilities a user writes in a `{...}` set
    * (`Console`/`Log`/`Dep`/`Throw`/`Abort`/`State`/`Inf`), the sequencing machinery (`Effect`/`Sync`), and each
    * effect's carrier representation (`ThrowCarrier`/`AbortCarrier`/`StateCarrier`). `Console`/`Log`/`Dep` are
    * auto-imported from here (see [[defaultSystemModules]]); the rest are imported explicitly or resolved by FQN. The
    * `Effect` ability's FQN is read by [[com.vanillasource.eliot.eliotc.effect.processor.EffectMachinery]]; the
    * `Console`/`Log`/`Inf` native leaves by the jvm `NativeImplementation`.
    */
  val effectPackage = Seq("eliot", "effect")

  /** The package for compiler-coordinated abilities that the checker resolves by name but that are kept out of the
    * user-facing `eliot.lang` prelude (the `java.lang` analogue) and intentionally NOT auto-imported (see
    * [[defaultSystemModules]]). These are *open* extension points — user/library types may add instances — so they
    * remain ordinarily importable (`import eliot.compiler.Coerce`), unlike the closed desugaring machinery in
    * [[compilerInternalPackage]]. Holds `Coerce` (check-mode widening), `Combine` (covariant join), and `Type` (the
    * type of every type — the resolver maps the bare name `Type` straight to its FQN, so it needs no auto-import and
    * the surface `[]`/kind sugar covers every ordinary use); the checker reaches their FQNs via
    * [[com.vanillasource.eliot.eliotc.module.fact.WellKnownTypes]].
    */
  val compilerPackage = Seq("eliot", "compiler")

  /** The package for compiler-internal desugaring machinery that user code never names directly — the
    * `PatternMatch`/`TypeMatch` abilities that the `match` / type-match syntax desugar onto. Deliberately kept out of the
    * user-facing `eliot.lang` prelude (the `java.lang` analogue) and intentionally NOT auto-imported (see
    * [[defaultSystemModules]]): compiler-generated `implement` markers reference these abilities by fixed FQN
    * (`ValueResolver.compilerInternalAbilities`), so user code never has them in scope.
    */
  val compilerInternalPackage = compilerPackage :+ "internal"

  val systemFunctionModuleName: ModuleName = ModuleName(defaultSystemPackage, "Function")
  // TODO: Unit is no longer here, so we shouldn't refer to it...
  // This is used to determine what to automatically import, but this should work differently.
  // NOTE: anything added here is auto-imported into every module, so the test harness must provide a matching stub
  // (see ProcessorTest's default systemImports). `Int`/`Runtime` are ambient as of the Phase-6 literal desugar: every
  // value-position integer literal `n` is rewritten to `integerLiteral[n] : Int[n, n]` (`CoreExpressionConverter`), so
  // `integerLiteral` (Runtime) and `Int` must resolve in every module. Code therefore must NOT import them explicitly
  // (that would double-import and shadow).
  val defaultSystemModules                 =
    Seq(
      "Function",
      "Unit",
      "String",
      "BigInteger",
      "IO",
      "Int",
      "Runtime"
    ).map(ModuleName(defaultSystemPackage, _)) ++
      // `Console`/`Log`/`Dep` are the public effect abilities users name ambiently: their operations and the `{...}`
      // sugar must resolve in every module with no import (the old top-level `println` lived in the ambient `String`
      // module). They live in the `eliot.effect` package ([[effectPackage]]). The internal effect machinery
      // (`Effect`/`Sync`) is NOT ambient — it is imported by the platform layer and referenced by FQN from the
      // effect-desugar phase.
      Seq(
        "Console",
        "Log",
        "Dep"
      ).map(ModuleName(effectPackage, _))
  // `PatternMatch`/`TypeMatch` are intentionally NOT here: they are desugaring machinery in the
  // `eliot.compiler.internal` package that user code never names. Compiler-generated `implement` markers reference them
  // by fixed FQN (`ValueResolver.compilerInternalAbilities`), so they need no import.
  // `Type` is also intentionally NOT here: it lives in [[compilerPackage]] and the resolver maps the bare name `Type`
  // straight to `WellKnownTypes.typeFQN` (see `ValueResolver`), so it resolves with no import while staying importable.

}
