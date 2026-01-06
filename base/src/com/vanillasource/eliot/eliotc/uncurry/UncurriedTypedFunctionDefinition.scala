package com.vanillasource.eliot.eliotc.uncurry

import com.vanillasource.eliot.eliotc.resolve.fact.{ArgumentDefinition, GenericParameter, TypeReference}
import com.vanillasource.eliot.eliotc.source.content.Sourced

/**
 * A function definition with uncurried typed expression.
 *
 * This restores multi-parameter function definitions and multi-argument applications
 * that were curried during resolution.
 */
case class UncurriedTypedFunctionDefinition(
    name: Sourced[String],
    genericParameters: Seq[GenericParameter],
    parameters: Seq[ArgumentDefinition],
    returnType: TypeReference,
    body: Option[Sourced[UncurriedTypedExpression]]
)
