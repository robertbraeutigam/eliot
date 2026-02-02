package com.vanillasource.eliot.eliotc.uncurry2.fact

import com.vanillasource.eliot.eliotc.eval.fact.Value
import com.vanillasource.eliot.eliotc.source.content.Sourced

/** A parameter definition for an uncurried function.
  *
  * @param name
  *   The sourced name of the parameter
  * @param parameterType
  *   The concrete type of the parameter as a Value
  */
case class ParameterDefinition(
    name: Sourced[String],
    parameterType: Value
)
