package com.vanillasource.eliot.eliotc.monomorphize.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue.*
import com.vanillasource.eliot.eliotc.eval.fact.Value
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerAbort

/** Pure type-level inference for monomorphization. Matches polymorphic type signatures against concrete types to
  * determine type argument bindings. Does not perform expression transformation.
  */
object MonomorphicTypeInference {

  /** Infer concrete type arguments by matching the expected type against a polymorphic type signature. When
    * [[Expected.Synthesize]], no call-site bindings are produced and resolution falls back to the environment's type
    * parameter substitution.
    */
  def inferFromCallSite(
      evalValue: ExpressionValue,
      typeParams: Seq[(String, Value)],
      expected: Expected,
      env: MonoEnv,
      source: Sourced[?]
  ): CompilerIO[Seq[Value]] = {
    val body     = ExpressionValue.stripLeadingLambdas(evalValue)
    val bindings = expected match {
      case Expected.Check(tpe) => ExpressionValue.matchTypes(body, ConcreteValue(tpe))
      case Expected.Synthesize => Map.empty[String, ExpressionValue]
    }
    resolveTypeParams(typeParams, bindings, Map.empty, env.typeParamSubst, source)
  }

  /** Infer type arguments by matching the function's return type against the call-site expected type. Only called when
    * the expected type is known (from a [[Expected.Check]] context).
    */
  def inferFromReturnType(
      bodyType: ExpressionValue,
      callSiteType: Value,
      typeParams: Seq[(String, Value)],
      typeParamSubst: Map[String, Value],
      source: Sourced[?]
  ): CompilerIO[Seq[Value]] = {
    val returnType = ExpressionValue.extractFunctionParamAndReturn(bodyType).map(_._2).getOrElse(bodyType)
    val bindings   = ExpressionValue.matchTypes(returnType, ConcreteValue(callSiteType))
    resolveTypeParams(typeParams, bindings, Map.empty, typeParamSubst, source)
  }

  /** Infer type arguments from argument type, with fallback to deep return type matching against the expected type.
    * Handles curried applications where some type parameters only appear in the return position.
    */
  def inferFromArgumentAndReturn(
      bodyType: ExpressionValue,
      argType: Value,
      expected: Expected,
      typeParams: Seq[(String, Value)],
      typeParamSubst: Map[String, Value],
      source: Sourced[?]
  ): CompilerIO[Seq[Value]] = {
    val paramType      = ExpressionValue.extractFunctionParamAndReturn(bodyType).map(_._1).getOrElse(bodyType)
    val argBindings    = ExpressionValue.matchTypes(paramType, ConcreteValue(argType))
    val returnBindings = computeReturnBindings(bodyType, expected, typeParams, argBindings)
    resolveTypeParams(typeParams, argBindings, returnBindings, typeParamSubst, source)
  }

  private def computeReturnBindings(
      bodyType: ExpressionValue,
      expected: Expected,
      typeParams: Seq[(String, Value)],
      argBindings: Map[String, ExpressionValue]
  ): Map[String, ExpressionValue] =
    expected match {
      case Expected.Check(callSiteType) =>
        val unresolvedParams = typeParams.map(_._1).toSet -- argBindings.collect { case (k, _: ConcreteValue) => k }
        if (unresolvedParams.isEmpty) Map.empty
        else {
          val deepReturn   = ExpressionValue.extractDeepReturnType(bodyType)
          val deepBindings = ExpressionValue.matchTypes(deepReturn, ConcreteValue(callSiteType))
          if (unresolvedParams.forall(p => deepBindings.get(p).exists(_.isInstanceOf[ConcreteValue])))
            deepBindings
          else {
            ExpressionValue.extractAllReturnTypes(bodyType)
              .foldLeft(deepBindings) { (best, rt) =>
                val bindings     = ExpressionValue.matchTypes(rt, ConcreteValue(callSiteType))
                val resolved     = unresolvedParams.count(p => bindings.get(p).exists(_.isInstanceOf[ConcreteValue]))
                val bestResolved = unresolvedParams.count(p => best.get(p).exists(_.isInstanceOf[ConcreteValue]))
                if (resolved > bestResolved) bindings ++ best else best ++ bindings
              }
          }
        }
      case Expected.Synthesize         => Map.empty
    }

  private def resolveTypeParams(
      typeParams: Seq[(String, Value)],
      primaryBindings: Map[String, ExpressionValue],
      secondaryBindings: Map[String, ExpressionValue],
      typeParamSubst: Map[String, Value],
      source: Sourced[?]
  ): CompilerIO[Seq[Value]] =
    typeParams.traverse { (param, _) =>
      primaryBindings.get(param) match {
        case Some(ConcreteValue(v)) => v.pure[CompilerIO]
        case _                      =>
          secondaryBindings.get(param) match {
            case Some(ConcreteValue(v)) => v.pure[CompilerIO]
            case _                      =>
              typeParamSubst.get(param).map(_.pure[CompilerIO]).getOrElse(
                compilerAbort(source.as(s"Cannot infer type argument for parameter: $param"))
              )
          }
      }
    }
}
