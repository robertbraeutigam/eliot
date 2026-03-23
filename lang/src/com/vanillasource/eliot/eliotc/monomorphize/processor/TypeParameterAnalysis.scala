package com.vanillasource.eliot.eliotc.monomorphize.processor

import com.vanillasource.eliot.eliotc.eval.fact.{ExpressionValue, Value}

/** Analyzes an evaluated type expression to separate "all" type parameters (including phantom/constraint-only) from
  * "body-visible" type parameters (those that appear in the body after stripping leading lambdas).
  *
  * This distinction matters because:
  *   - Explicit type arguments map to all type params (e.g. `id[Int]`)
  *   - Inferred type arguments map only to body-visible params (e.g. `id(42)` infers `A = BigInteger`)
  *   - Phantom params like `[I: BigInteger]` are constraint-only and not inferred from usage
  */
case class TypeParameterAnalysis(
    allTypeParams: Seq[(String, Value)],
    bodyTypeParams: Seq[(String, Value)]
) {

  /** Build a type parameter substitution map from concrete type arguments.
    *
    * @param typeArgs
    *   The concrete type argument values.
    * @param hasExplicitArgs
    *   True if type arguments were explicitly provided (maps to allTypeParams), false if inferred (maps to
    *   bodyTypeParams).
    */
  def buildSubstitution(typeArgs: Seq[Value], hasExplicitArgs: Boolean): Map[String, Value] =
    if (hasExplicitArgs)
      allTypeParams.map(_._1).zip(typeArgs).toMap
    else
      bodyTypeParams.map(_._1).zip(typeArgs).toMap
}

object TypeParameterAnalysis {

  /** Analyze an evaluated type expression, extracting all leading lambda parameters and identifying which ones appear in
    * the body.
    */
  def fromEvaluatedType(typeExprValue: ExpressionValue): TypeParameterAnalysis = {
    val allTypeParams  = ExpressionValue.extractLeadingLambdaParams(typeExprValue)
    val bodyExprValue  = ExpressionValue.stripLeadingLambdas(typeExprValue)
    val bodyTypeParams = allTypeParams.filter((name, _) => ExpressionValue.containsVar(bodyExprValue, name))
    TypeParameterAnalysis(allTypeParams, bodyTypeParams)
  }
}
