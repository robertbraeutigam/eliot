package com.vanillasource.eliot.eliotc.uncurry2.fact

import com.vanillasource.eliot.eliotc.eval.fact.Value
import com.vanillasource.eliot.eliotc.module2.fact.ValueFQN
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}
import com.vanillasource.eliot.eliotc.source.content.Sourced

/** A monomorphic value that has been uncurried to its optimal arity based on usage statistics.
  *
  * @param vfqn
  *   The fully qualified value name
  * @param typeArguments
  *   Concrete type arguments (from monomorphization)
  * @param name
  *   The sourced name
  * @param signature
  *   The concrete ground type (Value) after uncurrying
  * @param parameters
  *   The uncurried parameter list (optimal arity)
  * @param returnType
  *   The return type after uncurrying
  * @param body
  *   Optional uncurried expression body
  * @param targetArity
  *   The selected optimal arity for this function
  */
case class Uncurried2Value(
    vfqn: ValueFQN,
    typeArguments: Seq[Value],
    name: Sourced[String],
    signature: Value,
    parameters: Seq[Parameter2Definition],
    returnType: Value,
    body: Option[Sourced[Uncurried2Expression.Expression]],
    targetArity: Int
) extends CompilerFact {
  override def key(): CompilerFactKey[Uncurried2Value] =
    Uncurried2Value.Key(vfqn, typeArguments)
}

object Uncurried2Value {
  case class Key(vfqn: ValueFQN, typeArguments: Seq[Value])
      extends CompilerFactKey[Uncurried2Value]
}
