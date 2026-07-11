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
  * intervals; an `if`/fold branch joins its arms via `Meta.join`; everything else — a parameter, a value reference, an
  * ordinary call/constructor result, a lambda body — is ⊤ (no entry, laid out as a bignum). Codegen reads these
  * intervals for representation selection ([[RefinementRepresentation]]), and the LSP hover reads them for value-range
  * hints.
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
  */
case class RefinementTable(
    vfqn: ValueFQN,
    typeArguments: Seq[GroundValue],
    intervals: Seq[RefinementTable.NodeInterval]
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
