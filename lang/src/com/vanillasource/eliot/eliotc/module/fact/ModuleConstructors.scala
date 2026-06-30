package com.vanillasource.eliot.eliotc.module.fact

import com.vanillasource.eliot.eliotc.platform.Platform
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}

/** The constructors a module declares — its `data` value constructors grouped by data type, plus its type
  * constructors.
  *
  * This is the single owner of "which constructors does this module declare" — the question the syntactic
  * ([[com.vanillasource.eliot.eliotc.matchdesugar.processor.DataMatchDesugarer]]) and native-emitting
  * ([[com.vanillasource.eliot.eliotc.monomorphize.processor.MatchNativesProcessor]]) halves of `match` desugaring, and
  * the JVM backend, each used to re-derive independently from the raw [[UnifiedModuleNames]] name set. Centralizing it
  * removes that duplicated scan-classify-order logic and the risk of the copies disagreeing.
  *
  * Value constructors are recognized as a `Qualifier.Default`, upper-cased name whose [[UnifiedModuleValue]] carries a
  * [[com.vanillasource.eliot.eliotc.core.fact.RoleHint.ValueConstructor]] hint (the sanctioned constructor shape read —
  * see [[com.vanillasource.eliot.eliotc.core.fact.RoleHint]]); within each data type they are sorted by source
  * declaration position, the handler order the dispatch native and exhaustiveness check rely on. Type constructors are
  * the `Qualifier.Type`, upper-cased names (the type-level function a `data`/`type` declaration introduces).
  *
  * @param byDataType
  *   Maps each data type's `Qualifier.Type` qualified name (as carried by `RoleHint.ValueConstructor.dataType`) to its
  *   value constructors in source-declaration order.
  * @param typeConstructors
  *   The module's type-constructor names (`Qualifier.Type`, upper-cased).
  */
case class ModuleConstructors(
    moduleName: ModuleName,
    byDataType: Map[QualifiedName, Seq[ValueFQN]],
    typeConstructors: Seq[ValueFQN],
    platform: Platform = Platform.Runtime
) extends CompilerFact {
  override def key(): CompilerFactKey[ModuleConstructors] = ModuleConstructors.Key(moduleName, platform)

  /** The value constructors of `dataType` (its `Qualifier.Type` qualified name), in source-declaration order; empty if
    * this module declares no constructors for it.
    */
  def of(dataType: QualifiedName): Seq[ValueFQN] = byDataType.getOrElse(dataType, Seq.empty)
}

object ModuleConstructors {
  case class Key(moduleName: ModuleName, platform: Platform = Platform.Runtime)
      extends CompilerFactKey[ModuleConstructors]
}
