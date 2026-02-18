package com.vanillasource.eliot.eliotc.implementation.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.implementation.fact.{AbilityImplementation, AbilityImplementationCheck}
import com.vanillasource.eliot.eliotc.core.fact.{QualifiedName, Qualifier}
import com.vanillasource.eliot.eliotc.eval.fact.{ExpressionValue, Value}
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, UnifiedModuleNames, ValueFQN}
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.SingleKeyTypeProcessor
import com.vanillasource.eliot.eliotc.resolve.fact.AbilityFQN
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerError
import com.vanillasource.eliot.eliotc.symbolic.fact.{QualifiedName as SymbolicQualifiedName, Qualifier as SymbolicQualifier, TypeCheckedValue, TypedExpression}

class AbilityImplementationProcessor extends SingleKeyTypeProcessor[AbilityImplementation.Key] with Logging {

  override protected def generateFact(key: AbilityImplementation.Key): CompilerIO[Unit] = {
    val abilityValueFQN                   = key.abilityValueFQN
    val candidateModules: Set[ModuleName] =
      Set(abilityValueFQN.moduleName) ++ key.typeArguments.flatMap(_.typeFQN.map(_.moduleName))

    for {
      abilityFQN  <- abilityValueFQN.name.qualifier match {
                       case Qualifier.Ability(name) => AbilityFQN(abilityValueFQN.moduleName, name).pure[CompilerIO]
                       case other                   => error[CompilerIO](s"expected Ability qualifier, got: $other") >> abort
                     }
      _           <- getFactOrAbort(AbilityImplementationCheck.Key(abilityFQN, key.typeArguments))
      candidates  <- candidateModules.toSeq.flatTraverse(module =>
                       findImplementationsInModule(module, abilityValueFQN.name.name, abilityFQN.abilityName)
                     )
      matching    <- candidates.flatTraverse(vfqn => verifyImplementation(vfqn, abilityFQN, key.typeArguments))
      deduplicated = matching.distinctBy(_.show)
      _           <- deduplicated match {
                       case Seq(implFQN) =>
                         registerFactIfClear(AbilityImplementation(abilityValueFQN, key.typeArguments, implFQN))
                       case Seq()        =>
                         for {
                           abilityChecked <- getFactOrAbort(TypeCheckedValue.Key(abilityValueFQN))
                           _              <- abilityChecked.runtime match {
                                              case Some(runtime) =>
                                                handleDefaultImplementation(abilityChecked, abilityFQN, key, runtime)
                                              case None          =>
                                                compilerError(
                                                  abilityChecked.name.as(
                                                    s"No ability implementation found for ability '${abilityFQN.abilityName}' with type arguments ${key.typeArguments
                                                        .map(_.show)
                                                        .mkString("[", ", ", "]")}."
                                                  )
                                                )
                                            }
                         } yield ()
                       case multiple     =>
                         for {
                           abstractChecked <- getFactOrAbort(TypeCheckedValue.Key(abilityValueFQN))
                           _               <-
                             compilerError(
                               abstractChecked.name.as(
                                 s"Multiple ability implementations found for ability '${abilityFQN.abilityName}' with type arguments ${key.typeArguments
                                     .map(_.show)
                                     .mkString("[", ", ", "]")}."
                               )
                             )
                         } yield ()
                     }
    } yield ()
  }

  private def handleDefaultImplementation(
      abilityChecked: TypeCheckedValue,
      abilityFQN: AbilityFQN,
      key: AbilityImplementation.Key,
      sourcedRuntime: Sourced[TypedExpression.Expression]
  ): CompilerIO[Unit] = {
    val typeBindings      = computeTypeBindings(abilityChecked.signature, key.typeArguments)
    val typeSubst         = (ev: ExpressionValue) =>
      typeBindings.foldLeft(ev) { case (acc, (name, value)) => ExpressionValue.substitute(acc, name, value) }
    val concreteSignature = typeBindings.foldLeft(ExpressionValue.stripUniversalTypeIntros(abilityChecked.signature)) {
      case (acc, (name, value)) => ExpressionValue.substitute(acc, name, value)
    }
    val concreteRuntime   = sourcedRuntime.map(substituteTypesInExpression(_, typeSubst))
    val syntheticFQN      = createSyntheticFQN(key)
    val syntheticName     = abilityChecked.name.as(
                              SymbolicQualifiedName(
                                abilityChecked.name.value.name,
                                SymbolicQualifier.AbilityImplementation(
                                  abilityFQN,
                                  key.typeArguments.map(ExpressionValue.ConcreteValue(_))
                                )
                              )
                            )
    for {
      _ <- registerFactIfClear(TypeCheckedValue(syntheticFQN, syntheticName, concreteSignature, Some(concreteRuntime)))
      _ <- registerFactIfClear(AbilityImplementation(key.abilityValueFQN, key.typeArguments, syntheticFQN))
    } yield ()
  }

  private def computeTypeBindings(signature: ExpressionValue, typeArguments: Seq[Value]): Map[String, ExpressionValue] =
    ExpressionValue
      .extractLeadingLambdaParams(signature)
      .map(_._1)
      .zip(typeArguments.map(ExpressionValue.ConcreteValue(_)))
      .toMap

  private def createSyntheticFQN(key: AbilityImplementation.Key): ValueFQN = {
    val typeArgStr = key.typeArguments.map(Value.valueUserDisplay.show).mkString("$$")
    ValueFQN(
      key.abilityValueFQN.moduleName,
      QualifiedName(s"${key.abilityValueFQN.name.name}$$default$$${typeArgStr}", Qualifier.Default)
    )
  }

  private def substituteTypesInExpression(
      expr: TypedExpression.Expression,
      typeSubst: ExpressionValue => ExpressionValue
  ): TypedExpression.Expression =
    expr match {
      case TypedExpression.FunctionApplication(target, arg)            =>
        TypedExpression.FunctionApplication(
          target.map(_.transformTypes(typeSubst)),
          arg.map(_.transformTypes(typeSubst))
        )
      case TypedExpression.FunctionLiteral(paramName, paramType, body) =>
        TypedExpression.FunctionLiteral(
          paramName,
          paramType.map(typeSubst),
          body.map(_.transformTypes(typeSubst))
        )
      case other                                                       => other
    }

  private def findImplementationsInModule(
      moduleName: ModuleName,
      functionName: String,
      abilityLocalName: String
  ): CompilerIO[Seq[ValueFQN]] =
    getFact(UnifiedModuleNames.Key(moduleName)).map {
      case None        => Seq.empty
      case Some(names) =>
        names.names.toSeq.collect {
          case qn @ QualifiedName(`functionName`, Qualifier.AbilityImplementation(abilityNameSrc, _))
              if abilityNameSrc.value == abilityLocalName =>
            ValueFQN(moduleName, qn)
        }
    }

  private def verifyImplementation(
      vfqn: ValueFQN,
      expectedAbilityFQN: AbilityFQN,
      expectedTypeArgs: Seq[Value]
  ): CompilerIO[Seq[ValueFQN]] =
    getFact(TypeCheckedValue.Key(vfqn)).map {
      case None          => Seq.empty
      case Some(checked) =>
        checked.name.value.qualifier match {
          case SymbolicQualifier.AbilityImplementation(resolvedAbilityFQN, params)
              if resolvedAbilityFQN == expectedAbilityFQN && extractValues(params) == expectedTypeArgs =>
            Seq(vfqn)
          case _ => Seq.empty
        }
    }

  private def extractValues(params: Seq[ExpressionValue]): Seq[Value] =
    params.collect { case ExpressionValue.ConcreteValue(v) => v }
}
