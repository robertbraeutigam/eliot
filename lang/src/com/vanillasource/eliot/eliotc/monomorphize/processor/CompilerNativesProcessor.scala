package com.vanillasource.eliot.eliotc.monomorphize.processor

import cats.Id
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.module.fact.{Qualifier, UnifiedModuleNames, ValueFQN}
import com.vanillasource.eliot.eliotc.monomorphize.fact.{BindingContribution, CompilerMonomorphicValue, ContributedBinding}
import com.vanillasource.eliot.eliotc.operator.fact.{OperatorResolvedExpression, OperatorResolvedValue}
import com.vanillasource.eliot.eliotc.platform.Platform
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.saturate.fact.SaturatedValue

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

  /** A **compile-time** value — one the compiler layer defines (concrete here) but the runtime pool leaves abstract, e.g.
    * the guard combinators `error`/`orError` — whose checking body performs ability dispatch (`raise`/`pure`) cannot be
    * reduced by the pure NbE evaluator against the abstract ability method (which has no binding), so its raw body would
    * stall on it. For such a value the compile-time reduction is the **compiler backend's** output — the
    * [[CompilerMonomorphicValue]] whose ability calls are resolved to the concrete impls and folded — built here into a
    * self-contained [[BindingContribution.Leaf]] ([[ReducedBindingClosure]]). Both tracks then get the reducible form:
    * the runtime track evaluates it in a type position (compiler-as-platform Increment E, the guard consumer), and the
    * compiler track inlines it when reducing another compile-time value (e.g. `error` inside `orError`).
    *
    * The gate is deliberately narrow. Reducing an ability-using value at compile time only makes sense for a genuine
    * compile-time value; a *runtime* value that merely uses effects (`catch`/`runStateToPair`, or any user `{Console}` body)
    * is runtime-concrete and reaches runtime-only leaves (`runThrow`), so eagerly reducing it would raise a spurious
    * native-leaf error. So the reduced path is taken only when the value is **runtime-abstract** (no runtime body) — the
    * compiler-only / redefined-in-compiler-layer case. Everything else keeps the ordinary raw-body
    * [[BindingContribution.Body]] path.
    *
    * On a missing reduction — the value did not produce a `CompilerMonomorphicValue` (an upstream compile error) — the
    * raw body path is kept as a fail-safe: it stays stuck on the abstract ability and surfaces a loud use-site mismatch,
    * never a silently wrong reduction.
    */
  override protected def generateFromKeyAndFact(
      key: ContributedBinding.Key,
      fact: SaturatedValue
  ): CompilerIO[ContributedBinding] =
    if (performsAbility(fact.value))
      runtimeConcrete(key.vfqn).ifM(
        super.generateFromKeyAndFact(key, fact),
        getFact(CompilerMonomorphicValue.Key(key.vfqn, Seq.empty)).flatMap {
          case Some(cmv) =>
            cmv.reduced match {
              case Some(reduced) =>
                ReducedBindingClosure
                  .buildBinding(key.vfqn, reduced.value, Platform.Compiler)
                  .map(sem => ContributedBinding(key.vfqn, label, Some(BindingContribution.Leaf(sem))))
              case None          => super.generateFromKeyAndFact(key, fact)
            }
          case None      => super.generateFromKeyAndFact(key, fact)
        }
      )
    else super.generateFromKeyAndFact(key, fact)

  /** Whether the value's checking body references any ability method (`Qualifier.Ability`). Read off the raw
    * operator-resolved body — the signal that its raw reduction would stall on an unresolved ability call.
    */
  private def performsAbility(value: OperatorResolvedValue): Boolean =
    value.checkingRuntime.exists { body =>
      OperatorResolvedExpression.foldValueReferences[Id, Boolean](body.value, false) { (acc, ref) =>
        acc || (ref.value.name.qualifier match {
          case _: Qualifier.Ability => true
          case _                    => false
        })
      }
    }

  /** Whether `vfqn` has a *runtime* implementation (a body in the runtime pool). A runtime-concrete value is a runtime
    * value — its reduction belongs to codegen, not compile time — so it is never compile-time-reduced here even when it
    * uses effects. Gated on runtime-pool membership (a quiet name-set read) so a compiler-only value is not probed and
    * does not trigger the runtime pool's "Could not find" abort.
    */
  private def runtimeConcrete(vfqn: ValueFQN): CompilerIO[Boolean] =
    getFact(UnifiedModuleNames.Key(vfqn.moduleName, Platform.Runtime))
      .map(_.exists(_.names.contains(vfqn.name)))
      .ifM(
        getFact(SaturatedValue.Key(vfqn, Platform.Runtime)).map(_.exists(_.value.runtime.isDefined)),
        false.pure[CompilerIO]
      )
}
