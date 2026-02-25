package com.vanillasource.eliot.eliotc.abilitycheck

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.core.fact.Qualifier as CoreQualifier
import com.vanillasource.eliot.eliotc.eval.fact.{ExpressionValue, Value}
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.implementation.fact.AbilityImplementation
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.operator.fact.{OperatorResolvedExpression, OperatorResolvedValue}
import com.vanillasource.eliot.eliotc.processor.CompilerIO.{CompilerIO, getFact, getFactOrAbort}
import com.vanillasource.eliot.eliotc.processor.common.TransformationProcessor
import com.vanillasource.eliot.eliotc.resolve.fact.AbilityFQN
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
      resolvedValue <- getFact(OperatorResolvedValue.Key(fact.vfqn))
      paramConstraints = resolvedValue.map(_.paramConstraints).getOrElse(Map.empty)
      resolvedBody  <- fact.runtime match {
                         case Some(runtime) =>
                           resolveAbilityRefs(TypedExpression(fact.signature, runtime.value), paramConstraints)
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
    * implementations. Emits a compiler error if an ability is called with abstract type parameters not covered by a
    * constraint.
    */
  private def resolveAbilityRefs(
      typedExpr: TypedExpression,
      paramConstraints: Map[String, Seq[OperatorResolvedValue.ResolvedAbilityConstraint]]
  ): CompilerIO[TypedExpression] =
    typedExpr.expression match {
      case TypedExpression.FunctionApplication(target, arg) =>
        for {
          newTarget <- resolveAbilityRefsInSourced(target, paramConstraints)
          newArg    <- resolveAbilityRefsInSourced(arg, paramConstraints)
        } yield TypedExpression(
          typedExpr.expressionType,
          TypedExpression.FunctionApplication(newTarget, newArg)
        )

      case TypedExpression.FunctionLiteral(paramName, paramType, body) =>
        for {
          newBody <- resolveAbilityRefsInSourced(body, paramConstraints)
        } yield TypedExpression(
          typedExpr.expressionType,
          TypedExpression.FunctionLiteral(paramName, paramType, newBody)
        )

      case TypedExpression.ValueReference(vfqn) if isAbilityRef(vfqn.value) =>
        resolveAbilityRef(vfqn, typedExpr.expressionType, paramConstraints)
          .map(implFQN => TypedExpression(typedExpr.expressionType, TypedExpression.ValueReference(vfqn.as(implFQN))))

      case _ => typedExpr.pure[CompilerIO]
    }

  private def resolveAbilityRefsInSourced(
      sourced: Sourced[TypedExpression],
      paramConstraints: Map[String, Seq[OperatorResolvedValue.ResolvedAbilityConstraint]]
  ): CompilerIO[Sourced[TypedExpression]] =
    resolveAbilityRefs(sourced.value, paramConstraints).map(sourced.as)

  private def isAbilityRef(vfqn: ValueFQN): Boolean =
    vfqn.name.qualifier.isInstanceOf[CoreQualifier.Ability]

  private def resolveAbilityRef(
      vfqn: Sourced[ValueFQN],
      concreteType: ExpressionValue,
      paramConstraints: Map[String, Seq[OperatorResolvedValue.ResolvedAbilityConstraint]]
  ): CompilerIO[ValueFQN] = {
    val abilityLocalName = vfqn.value.name.qualifier.asInstanceOf[CoreQualifier.Ability].name
    for {
      abilityChecked  <- getFactOrAbort(TypeCheckedValue.Key(vfqn.value))
      typeArgExprs     = extractAbilityTypeArgs(abilityChecked.signature, concreteType)
      hasParameterRefs = typeArgExprs.exists(containsParameterRef)
      result          <- if (hasParameterRefs) {
                           if (isProvedByConstraint(vfqn.value, typeArgExprs, paramConstraints))
                             vfqn.value.pure[CompilerIO]
                           else
                             compilerAbort[ValueFQN](
                               vfqn.as(
                                 s"Cannot prove ability '$abilityLocalName' is available for given type. (Ability implementations require concrete types for now)."
                               )
                             )
                         } else {
                           for {
                             impl <- getFactOrAbort(AbilityImplementation.Key(vfqn.value, typeArgExprs))
                           } yield impl.implementationFQN
                         }
    } yield result
  }

  /** Returns true if the ability call's type arguments are all covered by a matching ability constraint on the
    * enclosing function's generic parameters. Matching is done by parameter name for ParameterReference type args.
    */
  private def isProvedByConstraint(
      vfqn: ValueFQN,
      typeArgExprs: Seq[ExpressionValue],
      paramConstraints: Map[String, Seq[OperatorResolvedValue.ResolvedAbilityConstraint]]
  ): Boolean = {
    val calledAbilityFQN = AbilityFQN(
      vfqn.moduleName,
      vfqn.name.qualifier.asInstanceOf[CoreQualifier.Ability].name
    )
    paramConstraints.values.flatten.exists { c =>
      c.abilityFQN == calledAbilityFQN &&
      c.typeArgs.length == typeArgExprs.length &&
      c.typeArgs.zip(typeArgExprs).forall {
        case (OperatorResolvedExpression.ParameterReference(nameSrc), ExpressionValue.ParameterReference(evName, _)) =>
          nameSrc.value == evName
        case _ => false
      }
    }
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
    val pattern        = ExpressionValue.stripUniversalTypeIntros(declarationSig)
    val bindings       = ExpressionValue.matchTypes(pattern, concreteSig, typeParamNames.contains)
    ExpressionValue
      .extractLeadingLambdaParams(declarationSig)
      .map((name, _) => bindings.getOrElse(name, ExpressionValue.ParameterReference(name, Value.Type)))
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
}
