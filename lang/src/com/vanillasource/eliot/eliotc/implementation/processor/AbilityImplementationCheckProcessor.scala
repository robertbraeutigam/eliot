package com.vanillasource.eliot.eliotc.implementation.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.core.fact.{QualifiedName, Qualifier}
import com.vanillasource.eliot.eliotc.implementation.fact.AbilityImplementationCheck
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, UnifiedModuleNames, ValueFQN}
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.SingleKeyTypeProcessor
import com.vanillasource.eliot.eliotc.resolve.fact.AbilityFQN
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerError
import com.vanillasource.eliot.eliotc.symbolic.fact.{TypeCheckedValue, Qualifier as SymbolicQualifier}
import com.vanillasource.eliot.eliotc.symbolic.types.SymbolicType

class AbilityImplementationCheckProcessor extends SingleKeyTypeProcessor[AbilityImplementationCheck.Key] {

  override protected def generateFact(key: AbilityImplementationCheck.Key): CompilerIO[Unit] = {
    val abilityFQN                        = key.abilityFQN
    val typeArguments                     = key.typeArguments
    val candidateModules: Set[ModuleName] =
      Set(abilityFQN.moduleName) ++ typeArguments.flatMap(collectModuleNames)

    for {
      abilityMethods <- collectAbilityMethods(abilityFQN)
      implMethods    <- candidateModules.toSeq.flatTraverse(collectImplMethods(_, abilityFQN, typeArguments))
      _              <- implMethods match {
                          case Nil =>
                            abilityMethods.headOption.traverse_(m =>
                              compilerError(
                                m.name.as(
                                  s"The type parameter '${typeArguments.map(SymbolicType.symbolicTypeUserDisplay.show).mkString(", ")}' does not implement ability '${abilityFQN.abilityName}'."
                                )
                              )
                            )
                          case _   =>
                            checkCompleteness(abilityMethods, implMethods) >>
                              checkNoExtras(abilityMethods, implMethods) >>
                              checkSignatures(abilityMethods, implMethods)
                        }
      _              <- registerFactIfClear(AbilityImplementationCheck(abilityFQN, typeArguments))
    } yield ()
  }

  private def collectAbilityMethods(abilityFQN: AbilityFQN): CompilerIO[Seq[TypeCheckedValue]] =
    getFactOrAbort(UnifiedModuleNames.Key(abilityFQN.moduleName)).flatMap { names =>
      names.names.keys.toSeq
        .collect {
          case qn @ QualifiedName(_, Qualifier.Ability(name)) if name == abilityFQN.abilityName =>
            ValueFQN(abilityFQN.moduleName, qn)
        }
        .traverse(vfqn => getFactOrAbort(TypeCheckedValue.Key(vfqn)))
    }

  private def collectImplMethods(
      moduleName: ModuleName,
      abilityFQN: AbilityFQN,
      typeArguments: Seq[SymbolicType]
  ): CompilerIO[Seq[TypeCheckedValue]] =
    getFactOrAbort(UnifiedModuleNames.Key(moduleName)).flatMap { names =>
      names.names.keys.toSeq
        .collect {
          case qn @ QualifiedName(_, Qualifier.AbilityImplementation(abilityNameSrc, _))
              if abilityNameSrc.value == abilityFQN.abilityName =>
            ValueFQN(moduleName, qn)
        }
        .traverse(vfqn => getFactOrAbort(TypeCheckedValue.Key(vfqn)))
        .map(_.filter { checked =>
          checked.name.value.qualifier match {
            case SymbolicQualifier.AbilityImplementation(resolvedFQN, params) if resolvedFQN == abilityFQN =>
              val freeVarNames = SymbolicType.extractLeadingLambdaParams(checked.signature).toSet
              implMatchesQuery(params, freeVarNames, typeArguments)
            case _                                                                                         => false
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

  /** Check that substituting the impl's qualifier pattern into the ability's declared function signature gives the same
    * signature as the implementation has. This validates both concrete implementations (pattern = concrete type) and
    * derived implementations (pattern = type expression with impl's own generic vars).
    *
    * Abstract ability type declarations (associated types) become unification variables in the ability's signature.
    * After substituting type parameters, any remaining unification variables are matched against the implementation's
    * concrete types via pattern matching, then verified for consistency.
    */
  private def checkSignatures(
      abilityMethods: Seq[TypeCheckedValue],
      implMethods: Seq[TypeCheckedValue]
  ): CompilerIO[Unit] = {
    val implByName = implMethods.map(m => m.name.value.name -> m).toMap
    abilityMethods
      .flatMap(abstractMethod => implByName.get(abstractMethod.name.value.name).map(abstractMethod -> _))
      .traverse_ { case (abstractMethod, implMethod) =>
        val abilityTypeParamNames = SymbolicType.extractLeadingLambdaParams(abstractMethod.signature)
        val implParams            = implMethod.name.value.qualifier match {
          case SymbolicQualifier.AbilityImplementation(_, params) => params
          case _                                                  => Seq.empty
        }
        val patternBindings       = abilityTypeParamNames.zip(implParams).toMap
        val strippedAbstract      = SymbolicType.stripUniversalTypeIntros(abstractMethod.signature)
        val expectedImplSig       = patternBindings.foldLeft(strippedAbstract) { case (acc, (name, param)) =>
          SymbolicType.substitute(acc, name, param)
        }
        val actualImplSig         = SymbolicType.stripUniversalTypeIntros(implMethod.signature)
        val unificationVarBindings =
          SymbolicType.matchTypes(expectedImplSig, actualImplSig, isUnificationVarName)
        val resolvedExpectedSig    = unificationVarBindings.foldLeft(expectedImplSig) { case (acc, (name, value)) =>
          SymbolicType.substitute(acc, name, value)
        }
        if (resolvedExpectedSig != actualImplSig)
          compilerError(
            implMethod.name.map(qn => s"Signature of implementation does not match the ability definition.")
          )
        else ().pure[CompilerIO]
      }
  }

  private def isUnificationVarName(name: String): Boolean = name.endsWith("$")

  private def implMatchesQuery(
      implParams: Seq[SymbolicType],
      freeVarNames: Set[String],
      queryArgs: Seq[SymbolicType]
  ): Boolean = {
    if (implParams.size != queryArgs.size) false
    else {
      val bindings = implParams.zip(queryArgs).foldLeft(Map.empty[String, SymbolicType]) { (acc, pair) =>
        acc ++ SymbolicType.matchTypes(pair._1, pair._2, freeVarNames.contains)
      }
      implParams.zip(queryArgs).forall { (implParam, queryArg) =>
        freeVarNames.foldLeft(implParam) { case (acc, name) =>
          SymbolicType
            .substitute(acc, name, bindings.getOrElse(name, SymbolicType.TypeVariable(name)))
        } == queryArg
      }
    }
  }

  private def collectModuleNames(st: SymbolicType): Seq[ModuleName] =
    st match {
      case SymbolicType.TypeReference(vfqn)  => Seq(vfqn.moduleName)
      case SymbolicType.TypeApplication(t, a) =>
        collectModuleNames(t.value) ++ collectModuleNames(a.value)
      case _                                  => Seq.empty
    }
}
