package com.vanillasource.eliot.eliotc.monomorphize.channel

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.monomorphize.fact.{GroundValue, MonomorphicExpression, MonomorphicValue}
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.TransformationProcessor
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerAbort

/** The **`WovenValue` codegen seam** — the step between checking and code generation that everything downstream
  * (`used` / `uncurry` / the jvm backend) reads instead of [[MonomorphicValue]].
  *
  * Since effects v6 it *rewrites* nothing. The Id-normalization it was built for — erasing the identity carrier `Id`
  * and the `pure`/`flatMap`/`runId` machinery the elaborator wrote at pure boundaries — has no subject: there is no
  * carrier, so no machinery is inserted and no pure code pays for one. What is left is the seam's other job, which is
  * to be the place three **preconditions** are checked before any bytecode is emitted:
  *
  *   - **effect accounting** — the bindings a value forwards are ones it declares ([[EffectAccountingProcessor]]);
  *   - **meta-transfer accounting** — a native leaf producing a meta-carrying type states what it does to it
  *     ([[MetaTransferAccountingProcessor]]);
  *   - **the woven re-check** — the body is type-checked once more on ground types, with no metavariables and no
  *     unification ([[WovenRecheck]]).
  *
  * Each is mandatory rather than advisory: a machine-generated core that nothing re-checks is exactly the
  * silent-miscompile surface the fail-safe rule forbids.
  */
class WovenValueProcessor()
    extends TransformationProcessor[MonomorphicValue.Key, WovenValue.Key](key =>
      MonomorphicValue.Key(key.vfqn, key.typeArguments)
    ) {

  override protected def generateFromKeyAndFact(
      key: WovenValue.Key,
      mv: MonomorphicValue
  ): CompilerIO[WovenValue] = {
    for {
      // Effect accounting as a **codegen precondition**: a value forwarding an implementation it does not declare
      // fails accounting, whose abort here blocks its `WovenValue` and so its codegen — a leak never reaches bytecode.
      _ <- getFactOrAbort(EffectAccounting.Key(mv.vfqn, mv.typeArguments))
      // Meta-transfer accounting (R2) as the same kind of codegen precondition (docs/total-meta-transfers.md §P2): a
      // native leaf producing a meta-carrying type without stating what it does to the meta-information fails
      // accounting, whose abort here blocks its `WovenValue` and so its codegen. Without it such a leaf silently
      // defaults its meta to ⊤, which is indistinguishable downstream from "nobody has computed this yet" — the
      // channel's one remaining source of untotality.
      _ <- getFactOrAbort(MetaTransferAccounting.Key(mv.vfqn, mv.typeArguments))
      // The **woven re-check**: the body that leaves this seam is type-checked once more, on ground types, with no
      // metavariables and no unification.
      _ <- assertWovenRechecks(mv, mv.signature, mv.runtime)
    } yield WovenValue(mv.vfqn, mv.typeArguments, mv.name, mv.signature, mv.runtime)
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
