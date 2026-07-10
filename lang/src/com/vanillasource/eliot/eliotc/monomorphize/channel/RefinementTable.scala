package com.vanillasource.eliot.eliotc.monomorphize.channel

import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.monomorphize.fact.GroundValue
import com.vanillasource.eliot.eliotc.pos.PositionRange
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}

/** The per-node refinement table of one monomorphic instance — the value channel of
  * `docs/bounds-as-refinements.md`, running in **shadow mode** (Step 2a).
  *
  * While `Int` still carries its bounds in the type, this table is a rider on the already-computed
  * [[com.vanillasource.eliot.eliotc.monomorphize.fact.MonomorphicValue]]: for every Int-typed node of the value's body
  * it records the interval the channel knows for that node. At an arithmetic transfer node the interval is *computed*
  * independently — by running the compiler-pool `Interval` arithmetic instance through the one NbE evaluator — and
  * asserted equal to the interval the type-parameter formulas produced (the shadow assertion, owned by
  * [[RefinementChannelProcessor]]). Everywhere else the interval is seeded from the node's own type argument, which in
  * shadow mode is the truth.
  *
  * The table is a first-order, serializable fact so it participates in the incremental cache and so codegen can read it
  * (exercised at Step 3 — representation selection from the channel). It carries no [[GroundValue.Type]]-only or
  * closure data, only interval endpoints ([[BigInt]] pairs) keyed by source position.
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
