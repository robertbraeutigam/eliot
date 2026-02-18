package com.vanillasource.eliot.eliotc.abilitycheck

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.core.fact.Qualifier as CoreQualifier
import com.vanillasource.eliot.eliotc.eval.fact.{ExpressionValue, Value}
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.implementation.fact.AbilityImplementation
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.processor.CompilerIO.{CompilerIO, getFactOrAbort}
import com.vanillasource.eliot.eliotc.processor.common.TransformationProcessor
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerAbort
import com.vanillasource.eliot.eliotc.symbolic.fact.{TypeCheckedValue, TypedExpression}

class AbilityCheckProcessor
    extends TransformationProcessor[TypeCheckedValue.Key, AbilityCheckedValue.Key](key =>
      TypeCheckedValue.Key(key.vfqn)
    )
    with Logging {
  override protected def generateFromKeyAndFact(
      key: AbilityCheckedValue.Key,
      fact: TypeCheckedValue
  ): CompilerIO[AbilityCheckedValue] =
    for {
      resolvedBody <- fact.runtime match {
                        case Some(runtime) =>
                          resolveAbilityRefs(TypedExpression(fact.signature, runtime.value))
                            .map(_.expression)
                            .map(runtime.as)
                            .map(Some.apply)
                        case None          => None.pure[CompilerIO]
                      }
    } yield AbilityCheckedValue(
      fact.vfqn,
      fact.name,
      fact.signature,
      resolvedBody
    )

  /** Recursively traverse the typed expression and replace any ability function references with their concrete
    * implementations. Emits a compiler error if an ability is called with abstract type parameters.
    */
  private def resolveAbilityRefs(typedExpr: TypedExpression): CompilerIO[TypedExpression] =
    typedExpr.expression match {
      case TypedExpression.FunctionApplication(target, arg) =>
        for {
          newTarget <- resolveAbilityRefsInSourced(target)
          newArg    <- resolveAbilityRefsInSourced(arg)
        } yield TypedExpression(
          typedExpr.expressionType,
          TypedExpression.FunctionApplication(newTarget, newArg)
        )

      case TypedExpression.FunctionLiteral(paramName, paramType, body) =>
        for {
          newBody <- resolveAbilityRefsInSourced(body)
        } yield TypedExpression(
          typedExpr.expressionType,
          TypedExpression.FunctionLiteral(paramName, paramType, newBody)
        )

      case TypedExpression.ValueReference(vfqn) if isAbilityRef(vfqn.value) =>
        resolveAbilityRef(vfqn, typedExpr.expressionType)
          .map(implFQN => TypedExpression(typedExpr.expressionType, TypedExpression.ValueReference(vfqn.as(implFQN))))

      case _ => typedExpr.pure[CompilerIO]
    }

  private def resolveAbilityRefsInSourced(sourced: Sourced[TypedExpression]): CompilerIO[Sourced[TypedExpression]] =
    resolveAbilityRefs(sourced.value).map(sourced.as)

  private def isAbilityRef(vfqn: ValueFQN): Boolean =
    vfqn.name.qualifier.isInstanceOf[CoreQualifier.Ability]

  private def resolveAbilityRef(
      vfqn: Sourced[ValueFQN],
      concreteType: ExpressionValue
  ): CompilerIO[ValueFQN] = {
    val abilityLocalName = vfqn.value.name.qualifier.asInstanceOf[CoreQualifier.Ability].name
    for {
      abilityChecked <- getFactOrAbort(TypeCheckedValue.Key(vfqn.value))
      typeArgExprs    = extractAbilityTypeArgs(abilityChecked.signature, concreteType)
      _              <- typeArgExprs.traverse_ { arg =>
                          if (containsParameterRef(arg))
                            compilerAbort[Unit](
                              vfqn.as(
                                s"Cannot call ability '$abilityLocalName' with abstract type parameter. Ability implementations require concrete types."
                              )
                            )
                          else ().pure[CompilerIO]
                        }
      valueArgs      <- typeArgExprs.traverse {
                          case ExpressionValue.ConcreteValue(v) => v.pure[CompilerIO]
                          case _                                =>
                            compilerAbort[Value](
                              vfqn.as(
                                s"Ability '$abilityLocalName' type argument could not be evaluated to a concrete type."
                              )
                            )
                        }
      impl           <- getFactOrAbort(AbilityImplementation.Key(vfqn.value, valueArgs))
    } yield impl.implementationFQN
  }

  /** Extract the concrete type arguments for an ability function call by matching the abstract function's
    * declaration-mode signature against the concrete instantiated type.
    *
    * For example, if the declaration signature is [A] -> A -> String and the concrete type is Int -> String, this
    * returns Seq(ConcreteValue(Int)).
    */
  private def extractAbilityTypeArgs(
      declarationSig: ExpressionValue,
      concreteSig: ExpressionValue
  ): Seq[ExpressionValue] = {
    val typeParamNames = ExpressionValue.extractLeadingLambdaParams(declarationSig).map(_._1).toSet
    val pattern        = stripUniversalIntros(declarationSig)
    val bindings       = matchExpressionValues(pattern, concreteSig, typeParamNames)
    ExpressionValue
      .extractLeadingLambdaParams(declarationSig)
      .map((name, _) => bindings.getOrElse(name, ExpressionValue.ParameterReference(name, Value.Type)))
  }

  /** Match a pattern ExpressionValue (with type parameter placeholders) against a concrete ExpressionValue, returning a
    * map from type parameter names to their concrete bindings.
    */
  private def matchExpressionValues(
      pattern: ExpressionValue,
      concrete: ExpressionValue,
      typeParamNames: Set[String]
  ): Map[String, ExpressionValue] =
    (pattern, concrete) match {
      case (ExpressionValue.ParameterReference(name, _), _) if typeParamNames.contains(name)          =>
        Map(name -> concrete)
      case (ExpressionValue.FunctionType(p1, r1), ExpressionValue.FunctionType(p2, r2))               =>
        matchExpressionValues(p1, p2, typeParamNames) ++ matchExpressionValues(r1, r2, typeParamNames)
      case (ExpressionValue.FunctionApplication(t1, a1), ExpressionValue.FunctionApplication(t2, a2)) =>
        matchExpressionValues(t1, t2, typeParamNames) ++ matchExpressionValues(a1, a2, typeParamNames)
      case _                                                                                          => Map.empty
    }

  /** Returns true if the ExpressionValue contains any ParameterReference node, indicating it depends on a type variable
    * (either a universal from the enclosing function, or an unresolved unification var).
    */
  private def containsParameterRef(expr: ExpressionValue): Boolean =
    expr match {
      case ExpressionValue.ParameterReference(_, _)    => true
      case ExpressionValue.FunctionApplication(t, a)   => containsParameterRef(t) || containsParameterRef(a)
      case ExpressionValue.FunctionLiteral(_, _, body) => containsParameterRef(body)
      case _                                           => false
    }

  @scala.annotation.tailrec
  private def stripUniversalIntros(expr: ExpressionValue): ExpressionValue =
    expr match {
      case ExpressionValue.FunctionLiteral(_, Value.Type, body) => stripUniversalIntros(body)
      case other                                                => other
    }
}
