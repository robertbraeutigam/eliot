package com.vanillasource.eliot.eliotc.monomorphize.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.module.fact.{UnifiedModuleNames, ValueFQN}
import com.vanillasource.eliot.eliotc.platform.Platform
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*

/** Quiet two-pool membership probe for the leaf native contributors ([[DataTypeNativesProcessor]],
  * [[MatchNativesProcessor]]).
  *
  * A [[com.vanillasource.eliot.eliotc.monomorphize.fact.ContributedBinding]] fact is **not** platform-keyed — one
  * contribution serves both the runtime and the compiler platform merge ([[BindingMergerProcessor]]). Yet a leaf
  * contributor must read a *platform-keyed* source fact (the value's [[com.vanillasource.eliot.eliotc.operator.fact.
  * OperatorResolvedValue]], its [[com.vanillasource.eliot.eliotc.module.fact.ModuleConstructors]]) to build a
  * reduction. Requesting that value on a fixed pool a name is absent from would trip the
  * `UnifiedModuleValueProcessor` "Could not find" error as a build side effect — exactly the hazard
  * [[CompilerNativesProcessor.inCompilerPool]] guards against.
  *
  * So a contributor asks here *first*, and reads the platform-keyed source fact only on the returned pool. The pool is
  * found by reading only the quiet [[UnifiedModuleNames]] name set (never a value), preferring [[Platform.Runtime]] —
  * the pool nearly every name lives in — and falling back to [[Platform.Compiler]] so a future compiler-pool-only
  * `data` type still gets its constructor / match natives. A name that is a member of neither pool yields [[None]], and
  * the contributor then contributes `None` (totality: never aborts, never errors).
  */
object DeclaringPool {

  /** The platform pool that declares `vfqn` — [[Platform.Runtime]] if present there, else [[Platform.Compiler]] if
    * present there, else [[None]].
    */
  def of(vfqn: ValueFQN): CompilerIO[Option[Platform]] =
    isMember(vfqn, Platform.Runtime).ifM(
      (Platform.Runtime: Platform).some.pure[CompilerIO],
      isMember(vfqn, Platform.Compiler).map(Option.when(_)(Platform.Compiler))
    )

  private def isMember(vfqn: ValueFQN, platform: Platform): CompilerIO[Boolean] =
    getFactIfProduced(UnifiedModuleNames.Key(vfqn.moduleName, platform)).map(_.exists(_.names.contains(vfqn.name)))
}
