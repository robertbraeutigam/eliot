package com.vanillasource.eliot.eliotc.interpret.fact

import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.monomorphize.fact.GroundValue
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}

/** The result of running a pure, fully-applied monomorphic value at compile time.
  *
  * This is the stable interface (fact boundary) of the compiler-internal evaluation backend: the type checker requests
  * an [[EvaluatedValue]] to reduce a closed, pure type-level computation to a [[GroundValue]], without knowing how the
  * value is executed. The current implementation interprets the uncurried monomorphic IR; the interface stays intact if
  * that is later swapped for direct JVM compile-and-run.
  *
  * @param vfqn
  *   The value that was run.
  * @param typeArguments
  *   The monomorphic type arguments identifying the specialization (matches the value's [[com.vanillasource.eliot.
  *   eliotc.uncurry.fact.UncurriedMonomorphicValue]] key).
  * @param arguments
  *   The ground runtime arguments the value was applied to.
  * @param result
  *   The resulting ground value.
  */
case class EvaluatedValue(
    vfqn: ValueFQN,
    typeArguments: Seq[GroundValue],
    arguments: Seq[GroundValue],
    result: GroundValue
) extends CompilerFact {
  override def key(): CompilerFactKey[EvaluatedValue] =
    EvaluatedValue.Key(vfqn, typeArguments, arguments)
}

object EvaluatedValue {
  case class Key(vfqn: ValueFQN, typeArguments: Seq[GroundValue], arguments: Seq[GroundValue])
      extends CompilerFactKey[EvaluatedValue]
}
