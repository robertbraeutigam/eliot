package com.vanillasource.eliot.eliotc.namedvalues.fact

import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedValue
import com.vanillasource.eliot.eliotc.platform.Platform
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}

/** An [[OperatorResolvedValue]] whose runtime body has had every `namedValues[V](name)` call expanded into its
  * residual builder chain ([[com.vanillasource.eliot.eliotc.namedvalues.processor.NamedValuesRewriteProcessor]]). Sits
  * between the `operator` and `termination` phases of the value chain
  * (`OperatorResolvedValue` → `NamedValuesRewrittenValue` →
  * [[com.vanillasource.eliot.eliotc.termination.fact.RecursionCheckedValue]]), so the emitted references flow through
  * termination → effect → saturation → monomorphization like any hand-written code.
  *
  * Only the runtime body may change; the signature and every other field are carried through untouched (the rewrite is
  * body-local), so the *sideways* [[OperatorResolvedValue]] reads later phases perform for a callee's signature, fixity
  * or ability marker stay valid against the un-rewritten fact.
  *
  * @param value
  *   the operator-resolved value with its `namedValues` calls expanded, or unchanged when it used none.
  */
case class NamedValuesRewrittenValue(value: OperatorResolvedValue) extends CompilerFact {
  override def key(): CompilerFactKey[NamedValuesRewrittenValue] =
    NamedValuesRewrittenValue.Key(value.vfqn, value.platform)
}

object NamedValuesRewrittenValue {
  case class Key(vfqn: ValueFQN, platform: Platform = Platform.Runtime)
      extends CompilerFactKey[NamedValuesRewrittenValue]
}
