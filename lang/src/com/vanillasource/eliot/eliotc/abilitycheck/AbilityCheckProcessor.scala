package com.vanillasource.eliot.eliotc.abilitycheck

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.core.fact.{QualifiedName, Qualifier as CoreQualifier}
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.implementation.fact.AbilityImplementation
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.operator.fact.{OperatorResolvedExpression, OperatorResolvedValue}
import com.vanillasource.eliot.eliotc.processor.CompilerIO.{CompilerIO, getFact, getFactOrAbort}
import com.vanillasource.eliot.eliotc.processor.common.TransformationProcessor
import com.vanillasource.eliot.eliotc.resolve.fact.AbilityFQN
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerAbort
import com.vanillasource.eliot.eliotc.symbolic.fact.{SymbolicType, TypeCheckedValue, TypedExpression}

import scala.annotation.tailrec

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
      case TypedExpression.ValueReference(vfqn) if isAbilityRef(vfqn.value) =>
        resolveAbilityRef(vfqn, typedExpr.expressionType, paramConstraints)
          .map(implFQN => TypedExpression(typedExpr.expressionType, TypedExpression.ValueReference(vfqn.as(implFQN))))
      case _                                                                =>
        TypedExpression
          .mapChildrenM[CompilerIO](s => resolveAbilityRefs(s.value, paramConstraints).map(s.as))(
            typedExpr.expression
          )
          .map(TypedExpression(typedExpr.expressionType, _))
    }

  private def isAbilityRef(vfqn: ValueFQN): Boolean =
    vfqn.name.qualifier.isInstanceOf[CoreQualifier.Ability]

  private def resolveAbilityRef(
      vfqn: Sourced[ValueFQN],
      concreteType: SymbolicType,
      paramConstraints: Map[String, Seq[OperatorResolvedValue.ResolvedAbilityConstraint]]
  ): CompilerIO[ValueFQN] = {
    val abilityLocalName = vfqn.value.name.qualifier.asInstanceOf[CoreQualifier.Ability].name
    for {
      abilityChecked        <- getFactOrAbort(TypeCheckedValue.Key(vfqn.value))
      allTypeArgExprs        = extractAbilityTypeArgs(abilityChecked.signature, concreteType)
      abilityTypeParamCount <- countAbilityTypeParams(vfqn.value)
      abilityTypeArgs        = allTypeArgExprs.take(abilityTypeParamCount)
      allConcrete            = abilityTypeArgs.forall(hasConcreteTopLevelConstructor)
      allFullyConcrete       = abilityTypeArgs.forall(isFullyConcrete)
      result                <- if (allFullyConcrete) {
                                 getFactOrAbort(AbilityImplementation.Key(vfqn.value, abilityTypeArgs))
                                   .map(_.implementationFQN)
                               } else if (allConcrete) {
                                 // Top-level type constructors are concrete but contain type parameters.
                                 // Keep the ability VFQN and let monomorphize resolve when types are fully concrete.
                                 vfqn.value.pure[CompilerIO]
                               } else if (isProvedByConstraint(vfqn.value, abilityTypeArgs, paramConstraints)) {
                                 vfqn.value.pure[CompilerIO]
                               } else {
                                 compilerAbort[ValueFQN](
                                   vfqn.as(
                                     s"Cannot prove ability '$abilityLocalName' is available for given type."
                                   )
                                 )
                               }
    } yield result
  }

  private def countAbilityTypeParams(vfqn: ValueFQN): CompilerIO[Int] = {
    val abilityName = vfqn.name.qualifier.asInstanceOf[CoreQualifier.Ability].name
    val markerVFQN  =
      ValueFQN(vfqn.moduleName, QualifiedName(abilityName, CoreQualifier.Ability(abilityName)))
    getFactOrAbort(TypeCheckedValue.Key(markerVFQN))
      .map(marker => SymbolicType.extractLeadingLambdaParams(marker.signature).size)
  }

  /** Returns true if the ability call's type arguments are all covered by a matching ability constraint on the
    * enclosing function's generic parameters. Matching is done by parameter name for TypeVariable type args.
    */
  private def isProvedByConstraint(
      vfqn: ValueFQN,
      typeArgExprs: Seq[SymbolicType],
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
        case (OperatorResolvedExpression.ParameterReference(nameSrc), SymbolicType.TypeVariable(evName)) =>
          nameSrc.value == evName
        case _ => false
      }
    }
  }

  /** Extract the concrete type arguments for an ability function call by matching the abstract function's
    * declaration-mode signature against the concrete instantiated type.
    *
    * For example, if the declaration signature is [A] -> A -> String and the concrete type is Int -> String, this
    * returns Seq(TypeReference(Int)).
    */
  private def extractAbilityTypeArgs(
      declarationSig: SymbolicType,
      concreteSig: SymbolicType
  ): Seq[SymbolicType] = {
    val typeParamNames = SymbolicType.extractLeadingLambdaParams(declarationSig).toSet
    val pattern        = SymbolicType.stripUniversalTypeIntros(declarationSig)
    val bindings       = SymbolicType.matchTypes(pattern, concreteSig, typeParamNames.contains)
    SymbolicType
      .extractLeadingLambdaParams(declarationSig)
      .map(name => bindings.getOrElse(name, SymbolicType.TypeVariable(name)))
  }

  @tailrec
  private def hasConcreteTopLevelConstructor(expr: SymbolicType): Boolean =
    expr match {
      case SymbolicType.TypeReference(_)        => true
      case SymbolicType.LiteralType(_, _)       => true
      case SymbolicType.TypeApplication(target, _) => hasConcreteTopLevelConstructor(target.value)
      case _                                    => false
    }

  private def isFullyConcrete(expr: SymbolicType): Boolean =
    expr match {
      case SymbolicType.TypeReference(_)           => true
      case SymbolicType.LiteralType(_, _)          => true
      case SymbolicType.TypeApplication(target, arg) => isFullyConcrete(target.value) && isFullyConcrete(arg.value)
      case _                                       => false
    }
}
