package com.vanillasource.eliot.eliotc.monomorphize.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.ability.fact.AbilityImplementation
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.monomorphize.check.{MarkerGuardSignature, Track, TypeStackLoop}
import com.vanillasource.eliot.eliotc.monomorphize.domain.SemValue
import com.vanillasource.eliot.eliotc.monomorphize.fact.{CompilerMonomorphicValue, GroundValue, MonomorphicValue, NativeBinding}
import com.vanillasource.eliot.eliotc.platform.Platform
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedExpression.SignatureView
import com.vanillasource.eliot.eliotc.processor.common.TransformationProcessor
import com.vanillasource.eliot.eliotc.saturate.fact.SaturatedValue

/** Entry point for NbE-based type checking (monomorphize). Delegates to TypeStackLoop for the actual type checking
  * work. Reads the [[SaturatedValue]] (the operator-resolved value with parameter-position bare omittable references
  * generalized into explicit generic binders), not the raw `OperatorResolvedValue`.
  */
class MonomorphicTypeCheckProcessor
    extends TransformationProcessor[SaturatedValue.Key, MonomorphicValue.Key](key => SaturatedValue.Key(key.vfqn)) {

  private def fetchBinding(vfqn: ValueFQN): CompilerIO[Option[SemValue]] =
    getFactIfProduced(NativeBinding.Key(vfqn, Platform.Runtime)).map(_.map(_.semValue))

  override protected def generateFromKeyAndFact(
      key: MonomorphicValue.Key,
      saturatedValue: SaturatedValue
  ): CompilerIO[MonomorphicValue] = {
    // An ability-implementation marker is monomorphized only to discharge its `where` guard (ability-guards §2.3); its
    // pattern-argument types are not real value parameters, so they are stripped to leave binders + guard return.
    val value = MarkerGuardSignature.strippedForGuard(saturatedValue.value)
    for {
      // The Step-6 flip: read the value's *own* signature reduced to ground from its signature twin's compiler-track mono
      // (types are compile-time, so the signature is authoritative there) rather than walking it in place. Absent (a W3
      // calculated return whose twin declined, an ability-implementation marker whose `where` guard only the in-place
      // machinery reduces, or a signature not producible on the compiler track) keeps the in-place walk. Acyclic:
      // `CompilerMonomorphicValue` never reads back a runtime `MonomorphicValue`.
      //
      // Gated on **full arity** (one type argument per binder), because `TypeStackLoop.establishSignature` consumes the
      // injected signature only at full arity — a partial-arity key (an erased-generic ability-impl method, keyed at its
      // impl-resolution-scope args; signature-unification §4.7) falls to the in-place walk regardless. Reading the twin
      // there would mint a partial-arity `@Signature` fact (leftover binders defaulted to `Type`) that is immediately
      // discarded — pure waste on every build; skipping the read is behaviour-preserving (the value flows nowhere else).
      injectedSignature <- if (
                             MarkerGuardSignature.isMarker(value) ||
                             key.typeArguments.sizeIs != SignatureView.of(value.signature).binders.size
                           ) none[GroundValue].pure[CompilerIO]
                           else
                             getFactIfProduced(
                               CompilerMonomorphicValue.Key(key.vfqn.copy(name = key.vfqn.name.signatureTwin), key.typeArguments)
                             ).map(_.map(_.signature))
      result            <- TypeStackLoop.process(
                             key.typeArguments,
                             value,
                             fetchBinding = fetchBinding,
                             resolveAbility = resolveAbilityImpl,
                             track = Track.Runtime,
                             reduceInstance = ReducedBindingClosure.reduceInstance(_, _, _),
                             injectedSignature = injectedSignature
                           )
    } yield MonomorphicValue(
      key.vfqn,
      key.typeArguments,
      value.signature.as(key.vfqn.name),
      result.signature,
      result.body
    )
  }

  private def resolveAbilityImpl(
      vfqn: ValueFQN,
      typeArgs: Seq[GroundValue]
  ): CompilerIO[Option[(ValueFQN, Seq[GroundValue])]] =
    getFactIfProduced(AbilityImplementation.Key(vfqn, typeArgs, Platform.Runtime)).map(
      _.flatMap(_.resolution.resolved)
    )
}
