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

  /** Infer concrete type arguments by matching the call-site type against a polymorphic type signature. */
  def inferFromCallSite(
      evalValue: ExpressionValue,
      typeParams: Seq[(String, Value)],
      callSiteType: Value,
      typeParamSubst: Map[String, Value],
      source: Sourced[?]
  ): CompilerIO[Seq[Value]] = {
    val body     = ExpressionValue.stripLeadingLambdas(evalValue)
    val bindings = ExpressionValue.matchTypes(body, ConcreteValue(callSiteType))

    typeParams.traverse { (param, _) =>
      bindings.get(param) match {
        case Some(ConcreteValue(v)) => v.pure[CompilerIO]
        case Some(_)                =>
          typeParamSubst.get(param) match {
            case Some(v) => v.pure[CompilerIO]
            case None    =>
              compilerAbort(source.as(s"Cannot infer type argument for parameter: $param (non-concrete match)"))
          }
        case None                   =>
          typeParamSubst.get(param) match {
            case Some(v) => v.pure[CompilerIO]
            case None    =>
              compilerAbort(source.as(s"Cannot infer type argument for parameter: $param"))
          }
      }
    }
  }

  /** Infer type arguments by matching the function's return type against the call-site expected type. */
  def inferFromReturnType(
      bodyType: ExpressionValue,
      callSiteType: Value,
      typeParams: Seq[(String, Value)],
      typeParamSubst: Map[String, Value],
      source: Sourced[?]
  ): CompilerIO[Seq[Value]] = {
    val returnType = ExpressionValue.extractFunctionParamAndReturn(bodyType).map(_._2).getOrElse(bodyType)
    val bindings   = ExpressionValue.matchTypes(returnType, ConcreteValue(callSiteType))

    typeParams.traverse { (param, _) =>
      bindings.get(param) match {
        case Some(ConcreteValue(v)) => v.pure[CompilerIO]
        case _                      =>
          typeParamSubst.get(param).map(_.pure[CompilerIO]).getOrElse(
            compilerAbort(source.as(s"Cannot infer type argument for parameter: $param"))
          )
      }
    }
  }

  /** Infer type arguments from argument type, with fallback to deep return type matching against the call-site type.
    * Handles curried applications where some type parameters only appear in the return position.
    */
  def inferFromArgumentAndReturn(
      bodyType: ExpressionValue,
      argType: Value,
      callSiteType: Value,
      typeParams: Seq[(String, Value)],
      typeParamSubst: Map[String, Value],
      source: Sourced[?]
  ): CompilerIO[Seq[Value]] = {
    val paramType   = ExpressionValue.extractFunctionParamAndReturn(bodyType).map(_._1).getOrElse(bodyType)
    val argBindings = ExpressionValue.matchTypes(paramType, ConcreteValue(argType))

    val returnBindings = if (callSiteType != Value.Type) {
      val unresolvedParams = typeParams.map(_._1).toSet -- argBindings.collect { case (k, _: ConcreteValue) => k }
      if (unresolvedParams.isEmpty) Map.empty[String, ExpressionValue]
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
    } else Map.empty

    typeParams.traverse { (param, _) =>
      argBindings.get(param) match {
        case Some(ConcreteValue(v)) => v.pure[CompilerIO]
        case Some(_)                =>
          resolveFromReturnOrSubst(
            param, returnBindings, typeParamSubst, source,
            s"Cannot infer type argument for parameter: $param (non-concrete match)"
          )
        case None                   =>
          resolveFromReturnOrSubst(
            param, returnBindings, typeParamSubst, source,
            s"Cannot infer type argument for parameter: $param"
          )
      }
    }
  }

  private def resolveFromReturnOrSubst(
      param: String,
      returnBindings: Map[String, ExpressionValue],
      typeParamSubst: Map[String, Value],
      source: Sourced[?],
      errorMsg: String
  ): CompilerIO[Value] =
    returnBindings.get(param) match {
      case Some(ConcreteValue(v)) => v.pure[CompilerIO]
      case _                      =>
        typeParamSubst.get(param).map(_.pure[CompilerIO]).getOrElse(
          compilerAbort(source.as(errorMsg))
        )
    }
}
