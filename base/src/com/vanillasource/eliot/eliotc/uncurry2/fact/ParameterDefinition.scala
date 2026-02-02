package com.vanillasource.eliot.eliotc.uncurry2.fact

import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue
import com.vanillasource.eliot.eliotc.source.content.Sourced

/** A parameter definition for an uncurried function.
  *
  * @param name
  *   The sourced name of the parameter
  * @param parameterType
  *   The type of the parameter
  */
case class ParameterDefinition(
    name: Sourced[String],
    parameterType: ExpressionValue
)
