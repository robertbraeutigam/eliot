package com.vanillasource.eliot.eliotc.uncurry.fact

import com.vanillasource.eliot.eliotc.monomorphize.fact.GroundValue
import com.vanillasource.eliot.eliotc.source.content.Sourced

/** A parameter definition with a concrete (monomorphic) type. */
case class MonomorphicParameterDefinition(
    name: Sourced[String],
    parameterType: GroundValue
)
