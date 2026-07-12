package com.vanillasource.eliot.eliotc.monomorphize.channel

import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.monomorphize.fact.GroundValue
import com.vanillasource.eliot.eliotc.pos.PositionRange
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}

/** The per-node refinement table of one monomorphic instance — the value channel of
  * `docs/bounds-as-refinements.md`, now a **productive flow analysis** (post-flag-day, Step 6-iii).
  *
  * `Int` no longer carries its bounds in the type, so this table is the *only* interval source: a rider on the
  * already-computed [[com.vanillasource.eliot.eliotc.monomorphize.fact.MonomorphicValue]] that *computes* each Int-typed
  * node's interval by flow, recording an entry only for the nodes it can pin. The propagation rules
  * ([[RefinementChannelProcessor]]): a literal seeds its singleton `[n, n]` (α); an arithmetic leaf
  * (`nativeAdd`/`nativeSubtract`/`nativeMultiply`) evaluates the leaf's `^Meta` transfer companion on the operand
  * intervals; a *merge* call (a callee with a `^Meta` **merge** companion, e.g. `fold`) reduces that companion to the
  * join of its arms; everything else — a parameter, a value reference, an ordinary call/constructor result, a lambda
  * body — is ⊤ (no entry, laid out as a bignum). The reconcile pass
  * ([[com.vanillasource.eliot.eliotc.reconcile.processor.ReconcileProcessor]]) stamps these intervals onto the body as
  * per-node metas, from which the JVM backend selects each `Int`'s machine width; the LSP hover reads them for
  * value-range hints.
  *
  * A merge also records **join-input edges** in [[joinInputs]]: the position of each argument the merge companion feeds
  * to the domain's `Meta.join` (a branch arm), targeting the merge result interval. The reconcile pass reads these to
  * re-encode each arm to the merged representation at the branch — the generic replacement for its old `Bool::fold`
  * special-case (`docs/generic-refinement-merges.md` Steps 4–5). The channel finds the join-input positions by a
  * structural read of the generic companion body (recognising the one primitive `Meta.join`), never by naming a branch
  * construct.
  *
  * The table is a first-order, serializable fact so it participates in the incremental cache and so codegen can read it.
  * It carries no [[GroundValue.Type]]-only or closure data, only interval endpoints ([[BigInt]] pairs) keyed by source
  * position.
  *
  * @param vfqn
  *   The value this table belongs to (the same instance identity as its [[MonomorphicValue]]).
  * @param typeArguments
  *   The concrete type arguments of the instance.
  * @param intervals
  *   One entry per Int-typed node of the runtime body, in walk order.
  * @param joinInputs
  *   One entry per branch-arm node: its source position and the merge result interval it must reconcile to (the reconcile
  *   pass re-encodes each arm to this). Empty for a body with no merges.
  */
case class RefinementTable(
    vfqn: ValueFQN,
    typeArguments: Seq[GroundValue],
    intervals: Seq[RefinementTable.NodeInterval],
    joinInputs: Seq[RefinementTable.NodeInterval] = Seq.empty
) extends CompilerFact {
  override def key(): CompilerFactKey[RefinementTable] =
    RefinementTable.Key(vfqn, typeArguments)
}

object RefinementTable {

  /** The channel's knowledge for one body node: the inclusive interval `[min, max]` of the `Int` value that node
    * evaluates to, at the node's source position.
    */
  case class NodeInterval(position: PositionRange, min: BigInt, max: BigInt)

  /** Keyed exactly like [[com.vanillasource.eliot.eliotc.monomorphize.fact.MonomorphicValue.Key]] — the same `vfqn` at
    * different type arguments is a different instance, hence a different table.
    */
  case class Key(vfqn: ValueFQN, typeArguments: Seq[GroundValue]) extends CompilerFactKey[RefinementTable]
}
