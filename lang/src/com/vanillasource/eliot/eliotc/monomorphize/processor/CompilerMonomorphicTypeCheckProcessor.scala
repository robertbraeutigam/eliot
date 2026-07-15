package com.vanillasource.eliot.eliotc.monomorphize.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.ability.fact.AbilityImplementation
import com.vanillasource.eliot.eliotc.module.fact.{Role, UnifiedModuleNames, ValueFQN}
import com.vanillasource.eliot.eliotc.monomorphize.check.{MarkerGuardSignature, Track, TypeStackLoop}
import com.vanillasource.eliot.eliotc.monomorphize.domain.SemValue
import com.vanillasource.eliot.eliotc.monomorphize.fact.{CompilerMonomorphicValue, GroundValue, NativeBinding}
import com.vanillasource.eliot.eliotc.platform.Platform
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.TransformationProcessor
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedValue
import com.vanillasource.eliot.eliotc.saturate.fact.SaturatedValue
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerError

/** The compiler track of NbE type checking: the analogue of [[MonomorphicTypeCheckProcessor]] that runs the *same*
  * [[TypeStackLoop]] over a value's `Platform.Compiler` [[SaturatedValue]], producing a [[CompilerMonomorphicValue]].
  *
  * It resolves everything in the compiler source pool: its bodies come from the compiler-preferred [[NativeBinding]]
  * (the merge already prefers the compiler-platform contributor), and its ability instances are queried under
  * `Platform.Compiler`. A compiler-platform value is *wholly type-level*, so every ability reference in it resolves in
  * the compiler platform — reaching a runtime-only native is an error (the native-leaf boundary), not a fall-through to
  * the runtime instance.
  *
  * This processor never mentions [[MonomorphicValue]] / its key, so the `compiler-mono → runtime-mono` fact edge cannot
  * exist; the two tracks are acyclic by construction.
  */
class CompilerMonomorphicTypeCheckProcessor
    extends TransformationProcessor[SaturatedValue.Key, CompilerMonomorphicValue.Key](key =>
      SaturatedValue.Key(key.vfqn, Platform.Compiler)
    ) {

  /** Fetch a name's compile-time reduction from the compiler pool, enforcing the **native-leaf boundary** (CP-C step c).
    *
    * A name with a compiler-platform [[NativeBinding]] reduces normally. A name with *none* is one of two things, and
    * they must be told apart — silently treating both as a stuck [[SemValue.VTopDef]] (the runtime track's correct
    * fallback, where the backend later emits the call) would let a runtime-only leaf corrupt a compile-time-computed
    * type, exactly the silent-wrong-typing the cornerstone forbids:
    *
    *   - **runtime-concrete / compiler-abstract** — it *has* a runtime realization (a jvm body or a runtime native
    *     leaf) but no compiler-platform definition. This is a *runtime-only value*; reaching it while reducing
    *     compile-time code is a hard error ("there is no backend here"), reported against the value being checked.
    *   - **body-less on both platforms** — an abstract type constructor (`Int`, body-less everywhere), an unresolved
    *     generic obligation, or an abstract ability method: no runtime binding either. These are legitimate compile-time
    *     normal forms / use-site-deferred obligations, so `None` is returned and evaluation leaves them stuck.
    *
    * The runtime-concreteness probe reads `NativeBinding(vfqn, Runtime)` — accessing runtime-defined source to *detect*
    * the leaf, never to *reduce* with it, so the `compiler-mono → runtime-mono` fact edge is not created (this processor
    * still cannot name `MonomorphicValue.Key`). It is gated on runtime-pool *membership* (a name-set read, like
    * [[CompilerNativesProcessor.inCompilerPool]]) so a compiler-only name — absent from the runtime pool entirely — is
    * not probed and does not trigger the runtime pool's "Could not find" abort; such a name is genuinely compiler-side
    * and stays `None`.
    */
  private def fetchBinding(source: Sourced[?])(vfqn: ValueFQN): CompilerIO[Option[SemValue]] =
    getFactIfProduced(NativeBinding.Key(vfqn, Platform.Compiler)).flatMap {
      case Some(nb) => Option(nb.semValue).pure[CompilerIO]
      case None     =>
        inRuntimePool(vfqn).ifM(
          getFactIfProduced(NativeBinding.Key(vfqn, Platform.Runtime)).flatMap {
            case Some(_) =>
              compilerError(
                source.as(s"Cannot use runtime-only value '${vfqn.name.name}' at compile time."),
                Seq(
                  s"'${vfqn.name.name}' has a runtime implementation but no compile-time definition on the compiler platform."
                )
              ) >> abort[Option[SemValue]]
            case None    => Option.empty[SemValue].pure[CompilerIO] // a runtime member abstract on *both* platforms
          },
          Option.empty[SemValue].pure[CompilerIO]                   // not a runtime member: compiler-only, not a leaf
        )
    }

  /** Whether `vfqn` is declared by its module in the runtime pool. Reads the [[UnifiedModuleNames]] name set (never the
    * value), so a name absent from the runtime pool yields `false` quietly instead of the "Could not find" abort a
    * direct value/binding request would raise.
    */
  private def inRuntimePool(vfqn: ValueFQN): CompilerIO[Boolean] =
    getFactIfProduced(UnifiedModuleNames.Key(vfqn.moduleName, Platform.Runtime)).map(_.exists(_.names.contains(vfqn.name)))

  override protected def generateFromKeyAndFact(
      key: CompilerMonomorphicValue.Key,
      saturatedValue: SaturatedValue
  ): CompilerIO[CompilerMonomorphicValue] = {
    // An ability-implementation marker is monomorphized only to discharge its `where` guard (ability-guards §2.3); its
    // pattern-argument types are not real value parameters, so they are stripped to leave binders + guard return.
    val value = MarkerGuardSignature.strippedForGuard(saturatedValue.value)
    // A `Signature`-role key demands the signature twin's own monomorphization (the signature split): kind-check the
    // signature body against the derived kind and reduce it, with no separate body to check (the `.runtime` slot is an
    // inert placeholder) and a W3 decline. A `Runtime`-role key is the ordinary compiler-track value mono. The role
    // rides `key.vfqn`, so the input `SaturatedValue` is already the role-appropriate twin's.
    val signatureOnly = key.vfqn.name.role == Role.Signature
    for {
      // The Step-6 flip: a `Runtime`-role value reads its *own* reduced ground signature from its signature twin's mono
      // rather than walking the signature in place. Absent (the signature twin computing itself, or a W3 twin that
      // declined) leaves the in-place walk. Acyclic: the signature twin never reads a runtime-role mono here.
      injectedSignature <- signatureTwinSignature(signatureOnly, key)
      result            <- TypeStackLoop.process(
                             key.typeArguments,
                             value,
                             fetchBinding = fetchBinding(value.name),
                             resolveAbility = resolveAbilityImpl,
                             track = Track.Compiler,
                             reduceInstance = ReducedBindingClosure.reduceInstance(_, _, _),
                             signatureOnly = signatureOnly,
                             injectedSignature = injectedSignature
                           )
    } yield CompilerMonomorphicValue(
      key.vfqn,
      key.typeArguments,
      value.signature.as(key.vfqn.name),
      result.signature,
      result.body
    )
  }

  /** The value's own reduced ground signature, read from `CompilerMonomorphicValue(v@Signature, args)` — the injected
    * signature the value mono re-inflates (signature-unification C1/C2). `None` only for a signature-twin key itself (it
    * computes its own signature); otherwise the twin is **mandatory** at every arity (a partial-arity key reads a
    * *parametric* signature with leftover `GroundValue.Param`s), so a missing twin aborts — its own mono already reported
    * the signature's errors.
    */
  private def signatureTwinSignature(
      signatureOnly: Boolean,
      key: CompilerMonomorphicValue.Key
  ): CompilerIO[Option[GroundValue]] =
    if (signatureOnly) none[GroundValue].pure[CompilerIO]
    else
      getFactOrAbort(
        CompilerMonomorphicValue.Key(key.vfqn.copy(name = key.vfqn.name.signatureTwin), key.typeArguments)
      ).map(cmv => Some(cmv.signature))

  /** Resolve an ability in the **compiler** pool: a compiler-track value is entirely compile-time, so all of its
    * ability references belong to the compiler platform.
    */
  private def resolveAbilityImpl(
      vfqn: ValueFQN,
      typeArgs: Seq[GroundValue]
  ): CompilerIO[Option[(ValueFQN, Seq[GroundValue])]] =
    getFactIfProduced(AbilityImplementation.Key(vfqn, typeArgs, Platform.Compiler)).map(
      _.flatMap(_.resolution.resolved)
    )
}
