package com.vanillasource.eliot.eliotc.implementation.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.core.fact.{QualifiedName, Qualifier}
import com.vanillasource.eliot.eliotc.eval.fact.{ExpressionValue, Value}
import com.vanillasource.eliot.eliotc.implementation.fact.AbilityImplementationCheck
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, UnifiedModuleNames, ValueFQN}
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.SingleKeyTypeProcessor
import com.vanillasource.eliot.eliotc.resolve.fact.AbilityFQN
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerError
import com.vanillasource.eliot.eliotc.symbolic.fact.{TypeCheckedValue, Qualifier as SymbolicQualifier}

class AbilityImplementationCheckProcessor extends SingleKeyTypeProcessor[AbilityImplementationCheck.Key] {

  override protected def generateFact(key: AbilityImplementationCheck.Key): CompilerIO[Unit] = {
    val abilityFQN                        = key.abilityFQN
    val typeArguments                     = key.typeArguments
    val candidateModules: Set[ModuleName] =
      Set(abilityFQN.moduleName) ++ typeArguments.flatMap(_.typeFQN.map(_.moduleName))

    for {
      abilityMethods <- collectAbilityMethods(abilityFQN)
      implMethods    <- candidateModules.toSeq.flatTraverse(collectImplMethods(_, abilityFQN, typeArguments))
      _              <- implMethods match {
                          case Nil =>
                            abilityMethods.headOption.traverse_(m =>
                              compilerError(
                                m.name.as(
                                  s"The type parameter '${typeArguments.map(Value.valueUserDisplay.show).mkString(", ")}' does not implement ability '${abilityFQN.abilityName}'."
                                )
                              )
                            )
                          case _   =>
                            checkCompleteness(abilityMethods, implMethods) >>
                              checkNoExtras(abilityMethods, implMethods) >>
                              checkSignatures(abilityMethods, implMethods, typeArguments)
                        }
      _              <- registerFactIfClear(AbilityImplementationCheck(abilityFQN, typeArguments))
    } yield ()
  }

  private def collectAbilityMethods(abilityFQN: AbilityFQN): CompilerIO[Seq[TypeCheckedValue]] =
    getFactOrAbort(UnifiedModuleNames.Key(abilityFQN.moduleName)).flatMap { names =>
      names.names.toSeq
        .collect {
          case qn @ QualifiedName(_, Qualifier.Ability(name)) if name == abilityFQN.abilityName =>
            ValueFQN(abilityFQN.moduleName, qn)
        }
        .traverse(vfqn => getFactOrAbort(TypeCheckedValue.Key(vfqn)))
    }

  private def collectImplMethods(
      moduleName: ModuleName,
      abilityFQN: AbilityFQN,
      typeArguments: Seq[Value]
  ): CompilerIO[Seq[TypeCheckedValue]] =
    getFactOrAbort(UnifiedModuleNames.Key(moduleName)).flatMap { names =>
      names.names.toSeq
        .collect {
          case qn @ QualifiedName(_, Qualifier.AbilityImplementation(abilityNameSrc, _))
              if abilityNameSrc.value == abilityFQN.abilityName =>
            ValueFQN(moduleName, qn)
        }
        .traverse(vfqn => getFactOrAbort(TypeCheckedValue.Key(vfqn)))
        .map(_.filter { checked =>
          checked.name.value.qualifier match {
            case SymbolicQualifier.AbilityImplementation(resolvedFQN, params)
                if resolvedFQN == abilityFQN && extractValues(params) == typeArguments =>
              true
            case _ => false
          }
        }.toSeq)
    }

  private def checkCompleteness(
      abilityMethods: Seq[TypeCheckedValue],
      implMethods: Seq[TypeCheckedValue]
  ): CompilerIO[Unit] = {
    val implMethodNames = implMethods.map(_.name.value.name).toSet
    val abstractMethods = abilityMethods.filter(_.runtime.isEmpty)
    abstractMethods
      .filterNot(m => implMethodNames.contains(m.name.value.name))
      .traverse_ { m =>
        implMethods.headOption match {
          case Some(implHead) =>
            val implSource = implHead.vfqn.name.qualifier.asInstanceOf[Qualifier.AbilityImplementation].name
            compilerError(implSource.as(s"Ability implementation is missing method '${m.name.value.name}'."))
          case None           =>
            compilerError(m.name.map(qn => s"Ability implementation is missing method '${qn.name}'."))
        }
      }
  }

  private def checkNoExtras(
      abilityMethods: Seq[TypeCheckedValue],
      implMethods: Seq[TypeCheckedValue]
  ): CompilerIO[Unit] = {
    val abilityMethodNames = abilityMethods.map(_.name.value.name).toSet
    implMethods
      .filterNot(m => abilityMethodNames.contains(m.name.value.name))
      .traverse_(m =>
        compilerError(
          m.name.map(qn => s"Method not defined in ability.")
        )
      )
  }

  private def checkSignatures(
      abilityMethods: Seq[TypeCheckedValue],
      implMethods: Seq[TypeCheckedValue],
      typeArguments: Seq[Value]
  ): CompilerIO[Unit] = {
    val implByName = implMethods.map(m => m.name.value.name -> m).toMap
    abilityMethods
      .flatMap(abstractMethod => implByName.get(abstractMethod.name.value.name).map(abstractMethod -> _))
      .traverse_ { case (abstractMethod, implMethod) =>
        val typeParamNames   =
          ExpressionValue.extractLeadingLambdaParams(abstractMethod.signature).map(_._1).toSet
        val strippedAbstract = ExpressionValue.stripUniversalTypeIntros(abstractMethod.signature)
        val bindings         =
          ExpressionValue.matchTypeVarBindings(strippedAbstract, implMethod.signature, typeParamNames)
        val expectedArgs     =
          ExpressionValue
            .extractLeadingLambdaParams(abstractMethod.signature)
            .map((name, _) => bindings.getOrElse(name, ExpressionValue.ParameterReference(name, Value.Type)))
        val concreteArgs     = typeArguments.map(ExpressionValue.ConcreteValue(_))
        if (expectedArgs != concreteArgs)
          compilerError(
            implMethod.name.map(qn => s"${qn.show}: Signature of '${qn.name}' does not match the ability definition.")
          )
        else ().pure[CompilerIO]
      }
  }

  private def extractValues(params: Seq[ExpressionValue]): Seq[Value] =
    params.collect { case ExpressionValue.ConcreteValue(v) => v }
}
