package com.vanillasource.eliot.eliotc.saturate.fact

import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedValue
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}

/** An [[OperatorResolvedValue]] whose source type stack has been *saturated*: every parameter-position bare reference
  * to an omittable (`auto`-marked) type constructor — e.g. a bare `Int` where `Int[MIN, MAX]` was meant — has been
  * rewritten into an explicit application over fresh generic binders, and those binders prepended to the value's own
  * generic prefix (see `docs/implicit-generics-plan.md`, W1). The synthesized binders are ordinary generic parameters,
  * so the existing "too few explicit type args → infer the rest" machinery solves them from each call's arguments with
  * no new inference.
  *
  * This is the fact every checker-phase reader of a value's *type stack* consumes instead of [[OperatorResolvedValue]]
  * (the monomorphize entry point and the `ValueReference` read in `Checker`). We cannot rewrite
  * [[OperatorResolvedValue]] in place — it is produced upstream by the `operator` phase, so feeding the rewrite back
  * would be a fact cycle.
  *
  * Return-position bare references are left untouched for W1 (they are *calculated*, handled in W3); a value with no
  * parameter-position bare omittable reference carries its [[OperatorResolvedValue]] through unchanged.
  *
  * @param value
  *   The (possibly rewritten) operator-resolved value, carrying the saturated type stack.
  */
case class SaturatedValue(value: OperatorResolvedValue) extends CompilerFact {
  override def key(): CompilerFactKey[SaturatedValue] = SaturatedValue.Key(value.vfqn)
}

object SaturatedValue {
  case class Key(vfqn: ValueFQN) extends CompilerFactKey[SaturatedValue]
}
