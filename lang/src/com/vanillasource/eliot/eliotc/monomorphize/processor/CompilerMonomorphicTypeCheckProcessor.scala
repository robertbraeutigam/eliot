package com.vanillasource.eliot.eliotc.monomorphize.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.ability.fact.AbilityImplementation
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.monomorphize.check.TypeStackLoop
import com.vanillasource.eliot.eliotc.monomorphize.domain.SemValue
import com.vanillasource.eliot.eliotc.monomorphize.fact.{CompilerMonomorphicValue, GroundValue, NativeBinding}
import com.vanillasource.eliot.eliotc.platform.Platform
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.TransformationProcessor
import com.vanillasource.eliot.eliotc.saturate.fact.SaturatedValue

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

  private def fetchBinding(vfqn: ValueFQN): CompilerIO[Option[SemValue]] =
    getFact(NativeBinding.Key(vfqn, Platform.Compiler)).map(_.map(_.semValue))

  override protected def generateFromKeyAndFact(
      key: CompilerMonomorphicValue.Key,
      saturatedValue: SaturatedValue
  ): CompilerIO[CompilerMonomorphicValue] =
    TypeStackLoop
      .process(
        key.typeArguments,
        saturatedValue.value,
        fetchBinding = fetchBinding,
        resolveAbility = resolveAbilityImpl,
        platform = Platform.Compiler
      )
      .map(result =>
        CompilerMonomorphicValue(
          key.vfqn,
          key.typeArguments,
          saturatedValue.value.typeStack.as(key.vfqn.name),
          result.signature,
          result.body
        )
      )

  /** Resolve an ability in the **compiler** pool, regardless of the `position` platform the call site passes: a
    * compiler-track value is entirely compile-time, so all of its ability references belong to the compiler platform.
    * (The `position` argument matters only in the runtime track, where type positions escalate to the compiler pool and
    * value positions stay on the runtime pool.)
    */
  private def resolveAbilityImpl(
      vfqn: ValueFQN,
      typeArgs: Seq[GroundValue],
      position: Platform
  ): CompilerIO[Option[(ValueFQN, Seq[GroundValue])]] =
    getFact(AbilityImplementation.Key(vfqn, typeArgs, Platform.Compiler)).map(
      _.map(impl => (impl.implementationFQN, impl.implementationTypeArgs))
    )
}
