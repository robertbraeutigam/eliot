package com.vanillasource.eliot.eliotc.monomorphize.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.module.fact.{UnifiedModuleNames, ValueFQN}
import com.vanillasource.eliot.eliotc.monomorphize.fact.ContributedBinding
import com.vanillasource.eliot.eliotc.platform.Platform
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*

/** The `compiler` native supplier: contributes the [[ContributedBinding]] under [[ContributedBinding.compilerLabel]] for
  * the compile-time reduction a name has in the **compiler** source pool (the compiler platform — base plus the
  * compiler-platform layer).
  *
  * The compiler is itself a platform (the "compiler as a platform" plan, CP3): a name can have a distinct concrete
  * implementation per platform, resolved against that platform's own source pool. This contributor is to the
  * **compiler** marker exactly what [[UserValueNativesProcessor]] is to the **runtime** marker — it reads the value's
  * own (checking) body via [[BodyContributorProcessor]] — with two differences: it reads the [[Platform.Compiler]]-marker
  * [[com.vanillasource.eliot.eliotc.saturate.fact.SaturatedValue]] (so it sees the compiler platform's redefinition,
  * e.g. the compile-time `Either` carrier, not the runtime layer's), and it is registered as a **native** label, so its
  * reduction is preferred for checking while the runtime layer's body is still what `TransparentBinding`/codegen reads
  * (the `add` pattern — one name, two platform implementations, kept apart by the marker).
  *
  * **Totality without spurious errors.** The [[BindingMergerProcessor]] consults this label for *every* name, including
  * the vast majority that have no compiler-platform definition (all user code, every runtime-only/jvm-only name). Those
  * are answered `None`. Crucially the `None` answer must not pollute the build: a name is asked about by membership
  * first — does its module's [[Platform.Compiler]] [[UnifiedModuleNames]] declare it? — and only a *member* triggers a
  * [[com.vanillasource.eliot.eliotc.saturate.fact.SaturatedValue]] request. This avoids the `UnifiedModuleValueProcessor`
  * "Could not find" abort for a name present in the runtime pool but absent from a shared compiler-pool module; a module
  * absent from the compiler path entirely aborts silently in `PathScanner` (the compiler path is an overlay, not the
  * build), so its `UnifiedModuleNames` is simply unavailable and membership is `false`.
  *
  * With no compiler-platform layer configured (the compiler path is base-only), every name is either body-less in base
  * or a non-member, so this contributes `None` everywhere and changes no resolution.
  */
class CompilerNativesProcessor extends BodyContributorProcessor(ContributedBinding.compilerLabel, Platform.Compiler) {

  // Only this label's queries belong to the compiler contributor; short-circuit other labels before any fetch. A
  // member of the compiler pool is built by BodyContributorProcessor from its compiler-marker SaturatedValue; a
  // non-member contributes the total `None` answer without requesting (and thus without erroring on) its value.
  override protected def generateFact(key: ContributedBinding.Key): CompilerIO[Unit] =
    if (key.label =!= ContributedBinding.compilerLabel) abort
    else
      inCompilerPool(key.vfqn).ifM(
        super.generateFact(key),
        registerFactIfClear(ContributedBinding(key.vfqn, ContributedBinding.compilerLabel, none))
      )

  /** Whether `vfqn` is declared by its module in the compiler pool. Reading [[UnifiedModuleNames]] (a name set) rather
    * than [[com.vanillasource.eliot.eliotc.saturate.fact.SaturatedValue]] keeps the non-member case quiet: an absent
    * module yields no fact (the compiler-path miss aborts silently in `PathScanner`) and a present module simply does
    * not list the name — neither path emits the "Could not find" error a direct value request would.
    */
  private def inCompilerPool(vfqn: ValueFQN): CompilerIO[Boolean] =
    getFact(UnifiedModuleNames.Key(vfqn.moduleName, Platform.Compiler)).map(_.exists(_.names.contains(vfqn.name)))
}
