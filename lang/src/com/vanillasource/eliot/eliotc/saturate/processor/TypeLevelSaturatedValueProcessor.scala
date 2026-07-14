package com.vanillasource.eliot.eliotc.saturate.processor

import cats.data.NonEmptySeq
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.core.fact.TypeStack
import com.vanillasource.eliot.eliotc.module.fact.WellKnownTypes
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedExpression.SignatureView
import com.vanillasource.eliot.eliotc.operator.fact.{OperatorResolvedExpression, OperatorResolvedValue}
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.TransformationProcessor
import com.vanillasource.eliot.eliotc.saturate.fact.SaturatedValue
import com.vanillasource.eliot.eliotc.source.content.Sourced

/** The type-levels-as-values derivation (Step A): answers `SaturatedValue.Key(v, platform, n)` for `n ≥ 1` — a value's
  * level-`n` *type expression*, presented as an ordinary [[SaturatedValue]] so the unchanged
  * [[com.vanillasource.eliot.eliotc.monomorphize.processor.CompilerMonomorphicTypeCheckProcessor]] /
  * [[com.vanillasource.eliot.eliotc.monomorphize.check.TypeStackLoop]] can check and reduce it like any small value.
  *
  * In the plan's numbering (which shifts the [[TypeStack]] index by one): level 0 is the runtime body; level `n ≥ 1` is
  * the host's `TypeStack.levels(n-1)`; the implicit top is the literal `Type`. The value at level `n` has **body** =
  * that level-`n` expression and **signature** = the level above. Because the fact engine dispatches every
  * `SaturatedValue.Key` to *both* this processor and [[SaturatedValueProcessor]], the two partition the key by level:
  * this one declines level 0, that one declines level `≥ 1`.
  *
  * The synthetic level value is built from the host's own signature via [[SignatureView]], so that running it through
  * the ordinary `process` reproduces exactly the host walk's per-instantiation reduction of that level (the equivalence
  * the plan's Step-A tests pin):
  *
  *   - The host level-`n` expression's *generic binders* become the synthetic value's own generic prefix — but with the
  *     inner type replaced by `Type` — so `applyTypeArgs` peels them (binding each argument in ρ) and the body is then
  *     checked against the kind `Type`.
  *   - The host level-`n` expression *with those binders stripped* becomes the synthetic **runtime body**: the type
  *     expression itself, its generic references now free and resolved to the ρ bindings. Reducing that body (compiler
  *     track) yields the instantiated level — the reduced signature at level 1, the reduced kind above.
  *   - The host's *higher* levels (`levels(n)…`, the kind chain of the level-`n` expression) ride along as the synthetic
  *     value's upper type-stack levels, giving the binder-check its expected kinds.
  *
  * Everything else (generics' ability constraints, role hint) is the host's, carried by `copy`.
  */
class TypeLevelSaturatedValueProcessor
    extends TransformationProcessor[SaturatedValue.Key, SaturatedValue.Key](key =>
      SaturatedValue.Key(key.vfqn, key.platform, 0)
    ) {

  /** Own only levels `≥ 1`. A level-0 key is [[SaturatedValueProcessor]]'s (and mapping it onto the level-0 input would
    * be a self-dependency); decline it.
    */
  override protected def generateFact(requestedKey: SaturatedValue.Key): CompilerIO[Unit] =
    if (requestedKey.typeLevel < 1) abort[Unit] else super.generateFact(requestedKey)

  override protected def generateFromKeyAndFact(
      key: SaturatedValue.Key,
      hostLevel0: SaturatedValue
  ): CompilerIO[SaturatedValue] =
    SaturatedValue(deriveLevel(hostLevel0.value, key.typeLevel), key.typeLevel).pure[CompilerIO]

  private def deriveLevel(host: OperatorResolvedValue, n: Int): OperatorResolvedValue = {
    val pos          = host.name
    val levels       = host.typeStack.value.levels.toSeq
    // Plan level n is the host's `levels(n-1)`; beyond the stack top is the literal `Type`.
    val levelExpr    = levels.lift(n - 1).getOrElse(typeRef(pos))
    val view         = SignatureView.of(host.typeStack.as(levelExpr))
    // Runtime body: the type expression, generic binders stripped so their references resolve to the ρ bindings.
    val runtimeBody  = view.copy(binders = Seq.empty).toExpression
    // Synthetic signature: the generic binders wrapping `Type`, so `applyTypeArgs` peels them and the body checks
    // against the kind `Type`. Its own kind chain is the host's `levels(n)…` (unchanged by dropping the inner type).
    val syntheticSig = view.withParameters(Seq.empty).withReturnType(typeRef(pos)).toExpression
    val upperLevels  = levels.drop(n)
    host.copy(
      runtime = Some(host.typeStack.as(runtimeBody)),
      typeStack = host.typeStack.as(TypeStack(NonEmptySeq(syntheticSig, upperLevels)))
    )
  }

  private def typeRef(pos: Sourced[?]): OperatorResolvedExpression =
    OperatorResolvedExpression.ValueReference(pos.as(WellKnownTypes.typeFQN))
}
