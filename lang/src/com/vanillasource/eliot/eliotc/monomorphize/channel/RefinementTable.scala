package com.vanillasource.eliot.eliotc.monomorphize.channel

import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.monomorphize.fact.GroundValue
import com.vanillasource.eliot.eliotc.pos.PositionRange
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}

/** The per-node refinement table of one monomorphic instance — the value channel of
  * `docs/bounds-as-refinements.md`, now a **productive flow analysis** (post-flag-day, Step 6-iii).
  *
  * `Int` no longer carries its bounds in the type, so this table is the *only* refinement source: a rider on the
  * already-computed [[com.vanillasource.eliot.eliotc.monomorphize.fact.MonomorphicValue]] that *computes* each node's
  * **meta value** (an opaque domain [[GroundValue]] — the type's `$Meta` structure) by flow, recording an entry only for
  * the nodes it can pin. The channel is domain-agnostic: the propagation rules ([[RefinementChannelProcessor]]) are a
  * literal seed (α — the one domain-origin point) and, at every other node, reducing the callee's `^Meta` companion on
  * the argument metas — a **transfer** (`nativeAdd^Meta`) or a **merge** (`fold^Meta`), computed the same way through the
  * one NbE evaluator, naming no leaf and no branch construct. Everything else — a parameter, a value reference, an
  * ordinary call/constructor result, a lambda body — is ⊤ (no entry, laid out as a bignum). The reconcile pass
  * ([[com.vanillasource.eliot.eliotc.reconcile.processor.ReconcileProcessor]]) stamps these meta values onto the body,
  * from which the JVM backend (which owns the `Int` domain) decodes each machine width *and* derives any re-encode; the
  * LSP hover unwraps them for value-range hints.
  *
  * The table is a first-order, serializable fact so it participates in the incremental cache and so codegen can read it;
  * a meta value is a [[GroundValue]] (the same first-order form used in fact keys), keyed by source position.
  *
  * @param vfqn
  *   The value this table belongs to (the same instance identity as its [[MonomorphicValue]]).
  * @param typeArguments
  *   The concrete type arguments of the instance.
  * @param metas
  *   One entry per tracked node of the runtime body, in walk order.
  */
case class RefinementTable(
    vfqn: ValueFQN,
    typeArguments: Seq[GroundValue],
    metas: Seq[RefinementTable.NodeMeta]
) extends CompilerFact {
  override def key(): CompilerFactKey[RefinementTable] =
    RefinementTable.Key(vfqn, typeArguments)
}

object RefinementTable {

  /** The channel's knowledge for one body node: the node's **meta value** — an opaque domain [[GroundValue]] (the type's
    * `$Meta` structure, e.g. `Int$Meta(Interval(lo, hi))`), at the node's source position. The channel neither builds
    * nor inspects its shape except to seed a literal; the consumers that own the domain (the JVM backend's width decode,
    * the LSP hover) unwrap it. A serializable first-order value ([[GroundValue]] is used in fact keys), so the table
    * participates in the incremental cache.
    */
  case class NodeMeta(position: PositionRange, meta: GroundValue)

  /** Keyed exactly like [[com.vanillasource.eliot.eliotc.monomorphize.fact.MonomorphicValue.Key]] — the same `vfqn` at
    * different type arguments is a different instance, hence a different table.
    */
  case class Key(vfqn: ValueFQN, typeArguments: Seq[GroundValue]) extends CompilerFactKey[RefinementTable]
}
