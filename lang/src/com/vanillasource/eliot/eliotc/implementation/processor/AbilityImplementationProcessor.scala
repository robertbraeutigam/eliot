package com.vanillasource.eliot.eliotc.implementation.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.implementation.fact.AbilityImplementation
import com.vanillasource.eliot.eliotc.core.fact.{QualifiedName, Qualifier}
import com.vanillasource.eliot.eliotc.eval.fact.{ExpressionValue, Value}
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, UnifiedModuleNames, ValueFQN}
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.SingleKeyTypeProcessor
import com.vanillasource.eliot.eliotc.resolve.fact.AbilityFQN
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerError
import com.vanillasource.eliot.eliotc.symbolic.fact.{Qualifier as SymbolicQualifier, TypeCheckedValue}

class AbilityImplementationProcessor extends SingleKeyTypeProcessor[AbilityImplementation.Key] {

  override protected def generateFact(key: AbilityImplementation.Key): CompilerIO[Unit] = {
    val abstractFQN       = key.abstractFunctionFQN
    val abilityLocalName  = abstractFQN.name.qualifier match {
      case Qualifier.Ability(name) => name
      case other                   => throw IllegalStateException(s"Expected Ability qualifier, got: $other")
    }
    val functionName      = abstractFQN.name.name
    val abilityModuleName = abstractFQN.moduleName
    val abilityFQN        = AbilityFQN(abilityModuleName, abilityLocalName)

    val candidateModules: Set[ModuleName] =
      Set(abilityModuleName) ++ key.typeArguments.flatMap(moduleOf)

    for {
      candidates  <- candidateModules.toSeq.flatTraverse(module =>
                       findImplementationsInModule(module, functionName, abilityLocalName)
                     )
      matching    <- candidates.flatTraverse(vfqn => verifyImplementation(vfqn, abilityFQN, key.typeArguments))
      deduplicated = matching.distinctBy(_.show)
      _           <- deduplicated match {
                       case Seq(implFQN) =>
                         registerFactIfClear(AbilityImplementation(abstractFQN, key.typeArguments, implFQN))
                       case Seq()        =>
                         for {
                           abstractChecked <- getFactOrAbort(TypeCheckedValue.Key(abstractFQN))
                           _               <-
                             compilerError(
                               abstractChecked.name.map(
                                 _.show + s": No ability implementation found for ability '$abilityLocalName' with type arguments ${key.typeArguments
                                     .map(_.show)
                                     .mkString("[", ", ", "]")}."
                               )
                             )
                         } yield ()
                       case multiple     =>
                         for {
                           abstractChecked <- getFactOrAbort(TypeCheckedValue.Key(abstractFQN))
                           _               <-
                             compilerError(
                               abstractChecked.name.map(
                                 _.show + s": Multiple ability implementations found for ability '$abilityLocalName' with type arguments ${key.typeArguments
                                     .map(_.show)
                                     .mkString("[", ", ", "]")}."
                               )
                             )
                         } yield ()
                     }
    } yield ()
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

  private def moduleOf(v: Value): Option[ModuleName] =
    v match {
      case Value.Structure(fields, _) =>
        fields.get("$typeName").collect { case Value.Direct(vfqn: ValueFQN, _) => vfqn.moduleName }
      case _                          => None
    }
}
