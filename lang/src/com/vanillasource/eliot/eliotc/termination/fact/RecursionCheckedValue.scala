package com.vanillasource.eliot.eliotc.termination.fact

import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedValue
import com.vanillasource.eliot.eliotc.platform.Platform
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}

/** An [[OperatorResolvedValue]] that has passed the no-recursion rule (termination M1): its body-level
  * value-reference graph contains no self or mutual cycle. The value is carried through unchanged — this fact only
  * certifies it — and it sits at the tail of the front-end value chain, feeding saturation directly
  * (`OperatorResolvedValue` → `RecursionCheckedValue` → [[com.vanillasource.eliot.eliotc.saturate.fact.SaturatedValue]];
  * effect verification is no longer a pre-mono gate here — it moved into the monomorphize phase).
  *
  * It is the structural gate that keeps a recursive definition out of monomorphization. A reported recursion error
  * trips `registerFactIfClear`, so a recursive value never produces this fact; because `SaturatedValueProcessor`
  * reads it as its sole input, such a value never reaches saturation, monomorphization or codegen (where it would
  * otherwise silently compile to a self-calling method or diverge the type-level computation). The gate is therefore
  * fail-safe by construction rather than relying on a consumer to remember to demand a check.
  *
  * @param value
  *   The operator-resolved value, certified free of body-level recursion and otherwise untouched.
  */
case class RecursionCheckedValue(value: OperatorResolvedValue) extends CompilerFact {
  override def key(): CompilerFactKey[RecursionCheckedValue] = RecursionCheckedValue.Key(value.vfqn, value.platform)
}

object RecursionCheckedValue {
  case class Key(vfqn: ValueFQN, platform: Platform = Platform.Runtime) extends CompilerFactKey[RecursionCheckedValue]
}
