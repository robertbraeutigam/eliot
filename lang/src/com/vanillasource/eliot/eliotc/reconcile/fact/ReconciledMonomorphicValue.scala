package com.vanillasource.eliot.eliotc.reconcile.fact

import com.vanillasource.eliot.eliotc.module.fact.{QualifiedName, ValueFQN}
import com.vanillasource.eliot.eliotc.monomorphize.fact.GroundValue
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.uncurry.fact.MonomorphicParameterDefinition

/** An uncurried monomorphic value whose body has been enriched by the refinement-reconcile pass
  * ([[com.vanillasource.eliot.eliotc.reconcile.processor.ReconcileProcessor]]).
  *
  * It carries the same value-level shape as
  * [[com.vanillasource.eliot.eliotc.uncurry.fact.UncurriedMonomorphicValue]] — the JVM backend reads `parameters` /
  * `returnType` for method descriptors exactly as before — and differs only in the body, whose expression tree is a
  * [[ReconciledMonomorphicExpression]] (per-node `meta` + inserted `Reconcile` nodes). This is the fact the backend
  * consumes for codegen; the width of each `Int` node is derived from its node `meta`, not from a lowered
  * representation type.
  */
case class ReconciledMonomorphicValue(
    vfqn: ValueFQN,
    typeArguments: Seq[GroundValue],
    arity: Int,
    name: Sourced[QualifiedName],
    signature: GroundValue,
    parameters: Seq[MonomorphicParameterDefinition],
    returnType: GroundValue,
    body: Option[Sourced[ReconciledMonomorphicExpression]]
) extends CompilerFact {
  override def key(): CompilerFactKey[ReconciledMonomorphicValue] =
    ReconciledMonomorphicValue.Key(vfqn, typeArguments, arity)
}

object ReconciledMonomorphicValue {
  case class Key(vfqn: ValueFQN, typeArguments: Seq[GroundValue], arity: Int)
      extends CompilerFactKey[ReconciledMonomorphicValue]
}
