package com.vanillasource.eliot.eliotc.uncurry.fact

import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.symbolic.types.SymbolicType

/** A parameter definition for an uncurried function.
  *
  * @param name
  *   The sourced name of the parameter
  * @param parameterType
  *   The type of the parameter
  */
case class ParameterDefinition(
    name: Sourced[String],
    parameterType: SymbolicType
)
