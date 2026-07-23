package com.vanillasource.eliot.eliotc.monomorphize.channel

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.monomorphize.fact.{GroundValue, MonomorphicExpression, MonomorphicValue}
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.TransformationProcessor
import com.vanillasource.eliot.eliotc.source.content.Sourced

/** The post-monomorphization **Id-normalization** stage (docs/effects-as-channel.md §6/§10, U1) — the one step that
  * occupies the `WovenValue` codegen seam between checking and codegen (`used`/`uncurry`/jvm read [[WovenValue]]).
  * On by default: today's checker already inserts `runId`/`Id` (`tryIdDefault`, discharge-to-pure) and would otherwise
  * ship the identity carrier `Id` to bytecode as a real data type with real allocations. This stage erases that pure
  * overhead totally so pure code recovers its efficient shape and no effect machinery ships for pure code:
  *
  *   - **U1a body rewrites + newtype representation** ([[IdNormalizer.normalizeValue]], `GroundValue.carrierFQN`):
  *     `runId(e) ⤳ e`, `Id(e) ⤳ e`, `pure@Effect[Id](e) ⤳ e`, `flatMap`/`map@Effect[Id](f, m) ⤳ f(m)`, the `runId`
  *     accessor body rewritten to the identity, and a first-class `Id` combinator reference eta-expanded to its lambda;
  *   - **U1b type/key erasure** ([[IdNormalizer.eraseIdTypes]]/[[IdNormalizer.eraseIdInBody]]): every `Id`-headed type
  *     erases to its payload in the signature, body node types, and reference type arguments — the last of which shifts
  *     the callee's demanded mono key so an `Id`-instantiation merges with its payload instantiation
  *     (`fold[Id[String]]` ≡ `fold[String]`). The WovenValue's *own* key is left as demanded (that demand is already
  *     erased), so key merging falls out of the demand shift.
  *
  * The stage is a **mandatory** compilation stage, not an optimization (the gaps-must-be-fail-safe rule), backed by the
  * [[warnIdResidue]] fail-safe: any `Id` machinery the rewrites failed to reach is reported (a warning during U1
  * bring-up, a hard build error from U4). Combined with the newtype representation, no `Id` allocation ships even if a
  * residue were ever missed.
  */
class WovenValueProcessor()
    extends TransformationProcessor[MonomorphicValue.Key, WovenValue.Key](key =>
      MonomorphicValue.Key(key.vfqn, key.typeArguments)
    )
    with Logging {

  override protected def generateFromKeyAndFact(
      key: WovenValue.Key,
      mv: MonomorphicValue
  ): CompilerIO[WovenValue] = {
    val normalized =
      mv.runtime.map(body => IdNormalizer.eraseIdInBody(IdNormalizer.normalizeValue(mv.vfqn, mv.signature, body)))
    val erasedSig  = IdNormalizer.eraseIdTypes(mv.signature)
    warnIdResidue(mv.vfqn, erasedSig, normalized)
      .as(WovenValue(mv.vfqn, mv.typeArguments, mv.name, erasedSig, normalized))
  }

  /** Warn on any `Id` residue left after normalization + erasure (the effects-as-channel §6 fail-safe — a warning in
    * U1, a hard build error from U4): a surviving `Id`-machinery *reference* (a first-class combinator the U1a rewrites
    * do not reach — kept safe by the newtype), or a surviving `Id[X]` *type* (a top-level carrier U1b erasure missed).
    */
  private def warnIdResidue(
      vfqn: ValueFQN,
      signature: GroundValue,
      body: Option[Sourced[MonomorphicExpression.Expression]]
  ): CompilerIO[Unit] = {
    val references  = body.map(b => IdNormalizer.residualIdReferences(b.value)).getOrElse(Seq.empty)
    val typeResidue = IdNormalizer.hasResidualIdType(signature, body)
    warn[CompilerIO](
      s"effects-as-channel: Id residue survived normalization in ${vfqn.show}: ${references.map(_.show).mkString(", ")}${if (typeResidue) " [Id-headed type]" else ""}"
    ).whenA(references.nonEmpty || typeResidue)
  }
}
