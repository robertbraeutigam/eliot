package com.vanillasource.eliot.eliotc.symbolic.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.ability.fact.AbilityImplementation
import com.vanillasource.eliot.eliotc.core.fact.{Qualifier as CoreQualifier, TypeStack}
import com.vanillasource.eliot.eliotc.eval.fact.{ExpressionValue, Value}
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.TransformationProcessor
import com.vanillasource.eliot.eliotc.resolve.fact.{Expression, ResolvedValue}
import com.vanillasource.eliot.eliotc.resolve.fact.{Qualifier as ResolveQualifier, QualifiedName as ResolveQualifiedName}
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.source.content.Sourced.{compilerAbort, compilerError}
import com.vanillasource.eliot.eliotc.symbolic.fact.*
import com.vanillasource.eliot.eliotc.symbolic.types.*
import com.vanillasource.eliot.eliotc.symbolic.types.TypeCheckState.*

/** Type checks resolved values by building type constraints and solving them through unification.
  *
  * The processor handles type stacks where each level describes the type of the level below:
  *   - The implicit top level is TypeType
  *   - Each higher level (above signature) must evaluate to ConcreteValue
  *   - Each level's value must have valueType matching the level above's value
  *   - Signature (level 0) and optional runtime become the final typed output
  *
  * After solving, ability function references in the body are replaced with their concrete implementations.
  * If an ability is called with abstract type parameters, a compiler error is emitted.
  */
class SymbolicTypeCheckProcessor
    extends TransformationProcessor[ResolvedValue.Key, TypeCheckedValue.Key](key => ResolvedValue.Key(key.vfqn))
    with Logging {

  override protected def generateFromKeyAndFact(
      key: TypeCheckedValue.Key,
      resolvedValue: ResolvedValue
  ): CompilerIO[TypeCheckedValue] =
    resolvedValue.runtime match {
      case Some(body) => typeCheckWithBody(resolvedValue, body)
      case None       => typeCheckWithoutBody(resolvedValue)
    }

  private def typeCheckWithBody(
      resolvedValue: ResolvedValue,
      body: Sourced[Expression]
  ): CompilerIO[TypeCheckedValue] =
    for {
      result <- (for {
                  (declaredType, typedLevels) <-
                    TypeStackBuilder.processStack(resolvedValue.typeStack).map { case (signatureType, typedStack) =>
                      (signatureType, typedStack.value.levels)
                    }
                  bodyResult                  <- TypeStackBuilder.inferBody(body)
                  // For constraint building, strip universal intros (type params) from declared type
                  // since the body's type won't have them - they're handled via universalVars
                  strippedDeclaredType         = stripUniversalIntros(declaredType)
                  _                           <- tellConstraint(
                                                   SymbolicUnification.constraint(
                                                     strippedDeclaredType,
                                                     body.as(bodyResult.expressionType),
                                                     "Type mismatch."
                                                   )
                                                 )
                  resolvedQualifierParams     <- resolveQualifierParams(resolvedValue.name)
                  constraints                 <- getConstraints
                  universalVars               <- getUniversalVars
                  unificationVars             <- getUnificationVars
                } yield (
                  declaredType,
                  typedLevels,
                  bodyResult,
                  constraints,
                  universalVars,
                  unificationVars,
                  resolvedQualifierParams
                ))
                  .runA(TypeCheckState())

      (declaredType, typedLevels, typedBody, constraints, universalVars, unificationVars, qualifierParams) = result

      _                   <- debug[CompilerIO](s"Constraints (of ${resolvedValue.vfqn.show}): ${constraints.show}")
      solution            <- constraints.solve(universalVars, unificationVars)
      _                   <- debug[CompilerIO](s"Solution (of ${resolvedValue.vfqn.show}): ${solution.show}")
      resolvedTypedLevels  = typedLevels.map(_.transformTypes(solution.substitute))
      resolvedTypedBody    = typedBody.transformTypes(solution.substitute)
      signatureType        = resolvedTypedLevels.head.expressionType
      resolvedQualifierParams = qualifierParams.map(solution.substitute)
      _                   <-
        debug[CompilerIO](
          s"Produced symbolic checked (of ${resolvedValue.vfqn.show}) signature: ${signatureType.show}, body: ${resolvedTypedBody.expression.show}"
        )
      resolvedBodyWithImpls <- resolveAbilityRefs(resolvedTypedBody)
    } yield TypeCheckedValue(
      resolvedValue.vfqn,
      convertQualifiedName(resolvedValue.name, resolvedQualifierParams),
      signatureType,
      Some(resolvedValue.typeStack.as(resolvedBodyWithImpls.expression))
    )

  private def typeCheckWithoutBody(
      resolvedValue: ResolvedValue
  ): CompilerIO[TypeCheckedValue] =
    for {
      result <- (for {
                  (signatureType, _) <- TypeStackBuilder
                                          .processStack(resolvedValue.typeStack)
                                          .map { case (signatureType, typedStack) =>
                                            (signatureType, typedStack.value.levels)
                                          }
                  qualifierParams    <- resolveQualifierParams(resolvedValue.name)
                } yield (signatureType, qualifierParams))
                  .runA(TypeCheckState())
      (signatureType, qualifierParams) = result
    } yield TypeCheckedValue(
      resolvedValue.vfqn,
      convertQualifiedName(resolvedValue.name, qualifierParams),
      signatureType,
      None
    )

  private def resolveQualifierParams(
      name: Sourced[ResolveQualifiedName]
  ): TypeGraphIO[Seq[ExpressionValue]] =
    name.value.qualifier match {
      case ResolveQualifier.AbilityImplementation(_, params) =>
        params.traverse { param =>
          TypeStackBuilder.processStack(name.as(TypeStack.of(param))).map(_._1)
        }
      case _                                                 => Seq.empty[ExpressionValue].pure[TypeGraphIO]
    }

  private def convertQualifiedName(
      name: Sourced[ResolveQualifiedName],
      qualifierParams: Seq[ExpressionValue]
  ): Sourced[QualifiedName] =
    name.map { n =>
      val qualifier = n.qualifier match {
        case ResolveQualifier.Default                       => Qualifier.Default
        case ResolveQualifier.Type                          => Qualifier.Type
        case ResolveQualifier.Ability(an)                   => Qualifier.Ability(an)
        case ResolveQualifier.AbilityImplementation(an, _)  => Qualifier.AbilityImplementation(an, qualifierParams)
      }
      QualifiedName(n.name, qualifier)
    }

  /** Recursively traverse the typed expression and replace any ability function references with their
    * concrete implementations. Emits a compiler error if an ability is called with abstract type parameters.
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
          .map(implFQN =>
            TypedExpression(typedExpr.expressionType, TypedExpression.ValueReference(vfqn.as(implFQN)))
          )

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
                              vfqn.as(s"Cannot call ability '$abilityLocalName' with abstract type parameter. Ability implementations require concrete types.")
                            )
                          else ().pure[CompilerIO]
                        }
      valueArgs      <- typeArgExprs.traverse {
                          case ExpressionValue.ConcreteValue(v) => v.pure[CompilerIO]
                          case _                                =>
                            compilerAbort[Value](
                              vfqn.as(s"Ability '$abilityLocalName' type argument could not be evaluated to a concrete type.")
                            )
                        }
      impl           <- getFactOrAbort(AbilityImplementation.Key(vfqn.value, valueArgs))
    } yield impl.implementationFQN
  }

  /** Extract the concrete type arguments for an ability function call by matching the abstract function's
    * declaration-mode signature against the concrete instantiated type.
    *
    * For example, if the declaration signature is [A] -> A -> String and the concrete type is Int -> String,
    * this returns Seq(ConcreteValue(Int)).
    */
  private def extractAbilityTypeArgs(
      declarationSig: ExpressionValue,
      concreteSig: ExpressionValue
  ): Seq[ExpressionValue] = {
    val typeParamNames = ExpressionValue.extractLeadingLambdaParams(declarationSig).map(_._1).toSet
    val pattern        = stripUniversalIntros(declarationSig)
    val bindings       = matchExpressionValues(pattern, concreteSig, typeParamNames)
    ExpressionValue.extractLeadingLambdaParams(declarationSig)
      .map((name, _) => bindings.getOrElse(name, ExpressionValue.ParameterReference(name, Value.Type)))
  }

  /** Match a pattern ExpressionValue (with type parameter placeholders) against a concrete ExpressionValue,
    * returning a map from type parameter names to their concrete bindings.
    */
  private def matchExpressionValues(
      pattern: ExpressionValue,
      concrete: ExpressionValue,
      typeParamNames: Set[String]
  ): Map[String, ExpressionValue] =
    (pattern, concrete) match {
      case (ExpressionValue.ParameterReference(name, _), _) if typeParamNames.contains(name) =>
        Map(name -> concrete)
      case (ExpressionValue.FunctionType(p1, r1), ExpressionValue.FunctionType(p2, r2))      =>
        matchExpressionValues(p1, p2, typeParamNames) ++ matchExpressionValues(r1, r2, typeParamNames)
      case (ExpressionValue.FunctionApplication(t1, a1), ExpressionValue.FunctionApplication(t2, a2)) =>
        matchExpressionValues(t1, t2, typeParamNames) ++ matchExpressionValues(a1, a2, typeParamNames)
      case _                                                                                   => Map.empty
    }

  /** Returns true if the ExpressionValue contains any ParameterReference node, indicating it depends
    * on a type variable (either a universal from the enclosing function, or an unresolved unification var).
    */
  private def containsParameterRef(expr: ExpressionValue): Boolean =
    expr match {
      case ExpressionValue.ParameterReference(_, _)    => true
      case ExpressionValue.FunctionApplication(t, a)   => containsParameterRef(t) || containsParameterRef(a)
      case ExpressionValue.FunctionLiteral(_, _, body) => containsParameterRef(body)
      case _                                            => false
    }

  /** Strip FunctionLiteral wrappers that represent universal type introductions. These have Value.Type as the parameter
    * type. This is used for constraint building where the body's type won't have them - they're handled via universalVars.
    */
  @scala.annotation.tailrec
  private def stripUniversalIntros(expr: ExpressionValue): ExpressionValue =
    expr match {
      case ExpressionValue.FunctionLiteral(_, Value.Type, body) => stripUniversalIntros(body)
      case other                                                => other
    }
}
