package com.vanillasource.eliot.eliotc.saturate.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.processor.CompilerIO
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.TransformationProcessor
import com.vanillasource.eliot.eliotc.row.fact.RowElaboratedValue
import com.vanillasource.eliot.eliotc.saturate.fact.SaturatedValue

/** Republishes a [[RowElaboratedValue]]'s operator-resolved value as a [[SaturatedValue]] — the fact every
  * checker-phase reader of a value's *signature* consumes (the monomorphize entry point and the `ValueReference` read
  * in `Checker`). It also derives each value's [[com.vanillasource.eliot.eliotc.saturate.fact.BinderRoles]] (lazily, on
  * the fact) for the monomorphization-keying plan.
  *
  * This phase once hosted the *implicit-generics* saturation (the retired `auto` feature): it rewrote every
  * parameter-position bare reference to an omittable (`auto`-marked) type constructor — e.g. a bare `Int` where
  * `Int[MIN, MAX]` was meant — into an explicit application over fresh generic binders. That machinery was removed when
  * its only intended target, a bounds-parameterized `Int`, became a single nullary type carrying its range as
  * refinement-channel meta-information: no user surface mints an omittable binder any more, and the sole remaining
  * `inferable` binder — the effect carrier — is a *leading* binder the row elaborator has already written explicitly by
  * the time this phase runs, never a bare under-applied constructor reference. With nothing left to saturate the phase
  * is a pass-through; it is kept as its own fact only because feeding a rewrite back into the upstream
  * [[com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedValue]] would be a fact cycle.
  */
class SaturatedValueProcessor
    extends TransformationProcessor[RowElaboratedValue.Key, SaturatedValue.Key](key =>
      RowElaboratedValue.Key(key.vfqn, key.platform)
    ) {

  override protected def generateFromKeyAndFact(
      key: SaturatedValue.Key,
      elaborated: RowElaboratedValue
  ): CompilerIO[SaturatedValue] =
    SaturatedValue(elaborated.value).pure[CompilerIO]
}
