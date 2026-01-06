package com.vanillasource.eliot.eliotc.uncurry

import com.vanillasource.eliot.eliotc.module.fact.FunctionFQN
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}

/**
 * Compiler fact representing a function with uncurried typed expressions.
 *
 * This fact is generated after type checking and reverses the currying
 * transformation, making function definitions and applications use
 * multiple parameters/arguments instead of nested single-parameter forms.
 */
case class UncurriedFunction(
    ffqn: FunctionFQN,
    definition: UncurriedTypedFunctionDefinition
) extends CompilerFact {
  override def key(): CompilerFactKey[?] = UncurriedFunction.Key(ffqn)
}

object UncurriedFunction {
  case class Key(ffqn: FunctionFQN) extends CompilerFactKey[UncurriedFunction]
}
