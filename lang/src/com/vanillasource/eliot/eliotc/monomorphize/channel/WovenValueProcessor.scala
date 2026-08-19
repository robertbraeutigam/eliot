package com.vanillasource.eliot.eliotc.monomorphize.channel

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.monomorphize.fact.{GroundValue, MonomorphicExpression, MonomorphicValue}
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.TransformationProcessor
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerAbort

/** The post-monomorphization **Id-normalization** stage (docs/effects-as-channel.md §6/§10, U1) — the one step that
  * occupies the `WovenValue` codegen seam between checking and codegen (`used`/`uncurry`/jvm read [[WovenValue]]). On
  * by default: today's checker already inserts `runId`/`Id` (`tryIdDefault`, discharge-to-pure) and would otherwise
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
  * [[assertNoIdResidue]] fail-safe: any `Id` machinery the rewrites failed to reach fails the build (a **hard error**
  * from U4-e — `Id` must not exist downstream of normalization, §9; a warning during U1 bring-up). Combined with the
  * newtype representation, no `Id` allocation ships even if a residue were ever missed.
  */
class WovenValueProcessor()
    extends TransformationProcessor[MonomorphicValue.Key, WovenValue.Key](key =>
      MonomorphicValue.Key(key.vfqn, key.typeArguments)
    ) {

  override protected def generateFromKeyAndFact(
      key: WovenValue.Key,
      mv: MonomorphicValue
  ): CompilerIO[WovenValue] = {
    val normalized =
      mv.runtime.map(body => IdNormalizer.eraseIdInBody(IdNormalizer.normalizeValue(mv.vfqn, mv.signature, body)))
    val erasedSig  = IdNormalizer.eraseIdTypes(mv.signature)
    for {
      // Effect accounting as a **codegen precondition** (docs/effects-as-channel.md §5/§10, U4-c-1): a value with an
      // undeclared effect fails accounting, whose abort here blocks its `WovenValue` and so its codegen — a leak never
      // reaches bytecode. Accounting verifies unconditionally (U4-c-2); for a valid program it always resolves and the
      // woven output is byte-identical.
      _ <- getFactOrAbort(EffectAccounting.Key(mv.vfqn, mv.typeArguments))
      // Meta-transfer accounting (R2) as the same kind of codegen precondition (docs/total-meta-transfers.md §P2): a
      // native leaf producing a meta-carrying type without stating what it does to the meta-information fails
      // accounting, whose abort here blocks its `WovenValue` and so its codegen. Without it such a leaf silently
      // defaults its meta to ⊤, which is indistinguishable downstream from "nobody has computed this yet" — the
      // channel's one remaining source of untotality.
      _ <- getFactOrAbort(MetaTransferAccounting.Key(mv.vfqn, mv.typeArguments))
      _ <- assertNoIdResidue(mv, erasedSig, normalized)
      // The **woven re-check** (effects-as-channel v4 §6, §9 standing rule 4, §11 P3): the body that leaves this seam
      // is type-checked once more, on ground types, with no metavariables and no unification. Landed on today's output
      // first, where it must be a no-op — the v3 elaborator writes the same monadic core a v4 seam lowering would, so a
      // rejection here is a rejection of the elaborator, not of the lowering. Mandatory, like `assertNoIdResidue`: a
      // machine-generated core that nothing re-checks is exactly the silent-miscompile surface the fail-safe rule
      // forbids.
      _ <- assertWovenRechecks(mv, erasedSig, normalized)
    } yield WovenValue(mv.vfqn, mv.typeArguments, mv.name, erasedSig, normalized)
  }

  /** Assert no `Id` residue remains after normalization + erasure (the effects-as-channel §6 fail-safe, a **hard
    * error** from U4-e — `Id` exists only between elaboration and normalization, nowhere downstream, §9). A surviving
    * `Id`-machinery *reference* (a first-class combinator the U1a rewrites did not reach) or a surviving `Id[X]` *type*
    * (a top-level carrier U1b erasure missed) is a normalizer-invariant violation and fails the build at the offending
    * value. The jvm newtype representation of `Id` (`Id[A] ≡ A`) still keeps any such residue a codegen no-op, so this
    * is a correctness *tripwire*, not a soundness gate; from U4-e the invariant is that it never fires. It was a
    * warning during U1 bring-up (docs/effects-as-channel.md §6/§9/§10).
    */
  private def assertNoIdResidue(
      mv: MonomorphicValue,
      signature: GroundValue,
      body: Option[Sourced[MonomorphicExpression.Expression]]
  ): CompilerIO[Unit] = {
    val references  = body.map(b => IdNormalizer.residualIdReferences(b.value)).getOrElse(Seq.empty)
    val typeResidue = IdNormalizer.hasResidualIdType(signature, body)
    compilerAbort[Unit](
      mv.name.as(
        s"effects-as-channel: Id residue survived normalization in ${mv.vfqn.show}: ${references
            .map(_.show)
            .mkString(", ")}${if (typeResidue) " [Id-headed type]" else ""}"
      )
    ).whenA(references.nonEmpty || typeResidue)
  }

  /** Report every disagreement [[WovenRecheck]] found in the woven body as a compiler error at the offending node, and
    * abort — a weave that does not type-check must not reach codegen.
    */
  private def assertWovenRechecks(
      mv: MonomorphicValue,
      signature: GroundValue,
      body: Option[Sourced[MonomorphicExpression.Expression]]
  ): CompilerIO[Unit] = {
    val problems = WovenRecheck.check(signature, body)
    problems.headOption.traverse_(problem => compilerAbort[Unit](problem.at.as(problem.message)))
  }
}
