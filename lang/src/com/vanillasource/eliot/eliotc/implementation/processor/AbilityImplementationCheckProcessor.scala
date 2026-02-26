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
      Set(abilityFQN.moduleName) ++ typeArguments.flatMap(collectExpressionModuleNames)

    for {
      abilityMethods <- collectAbilityMethods(abilityFQN)
      implMethods    <- candidateModules.toSeq.flatTraverse(collectImplMethods(_, abilityFQN, typeArguments))
      _              <- implMethods match {
                          case Nil =>
                            abilityMethods.headOption.traverse_(m =>
                              compilerError(
                                m.name.as(
                                  s"The type parameter '${typeArguments.map(ExpressionValue.expressionValueUserDisplay.show).mkString(", ")}' does not implement ability '${abilityFQN.abilityName}'."
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
      typeArguments: Seq[ExpressionValue]
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
            case SymbolicQualifier.AbilityImplementation(resolvedFQN, params)
                if resolvedFQN == abilityFQN =>
              val freeVarNames = ExpressionValue.extractLeadingLambdaParams(checked.signature).map(_._1).toSet
              implMatchesQuery(params, freeVarNames, typeArguments)
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

  /** Check that substituting the impl's qualifier pattern into the ability's declared function signature gives the same
    * signature as the implementation has. This validates both concrete implementations (pattern = concrete type) and
    * derived implementations (pattern = type expression with impl's own generic vars).
    */
  private def checkSignatures(
      abilityMethods: Seq[TypeCheckedValue],
      implMethods: Seq[TypeCheckedValue]
  ): CompilerIO[Unit] = {
    val implByName = implMethods.map(m => m.name.value.name -> m).toMap
    abilityMethods
      .flatMap(abstractMethod => implByName.get(abstractMethod.name.value.name).map(abstractMethod -> _))
      .traverse_ { case (abstractMethod, implMethod) =>
        val abilityTypeParamNames = ExpressionValue.extractLeadingLambdaParams(abstractMethod.signature).map(_._1)
        val implParams            = implMethod.name.value.qualifier match {
          case SymbolicQualifier.AbilityImplementation(_, params) => params
          case _                                                   => Seq.empty
        }
        val patternBindings       = abilityTypeParamNames.zip(implParams).toMap
        val strippedAbstract      = ExpressionValue.stripUniversalTypeIntros(abstractMethod.signature)
        val expectedImplSig       = patternBindings.foldLeft(strippedAbstract) { case (acc, (name, param)) =>
          ExpressionValue.substitute(acc, name, param)
        }
        val actualImplSig         = ExpressionValue.stripUniversalTypeIntros(implMethod.signature)
        if (expectedImplSig != actualImplSig)
          compilerError(
            implMethod.name.map(qn => s"${qn.show}: Signature of '${qn.name}' does not match the ability definition.")
          )
        else ().pure[CompilerIO]
      }
  }

  private def implMatchesQuery(
      implParams: Seq[ExpressionValue],
      freeVarNames: Set[String],
      queryArgs: Seq[ExpressionValue]
  ): Boolean = {
    if (implParams.size != queryArgs.size) false
    else {
      val bindings = implParams.zip(queryArgs).foldLeft(Map.empty[String, ExpressionValue]) { (acc, pair) =>
        acc ++ ExpressionValue.matchTypes(pair._1, pair._2, freeVarNames.contains)
      }
      implParams.zip(queryArgs).forall { (implParam, queryArg) =>
        freeVarNames.foldLeft(implParam) { case (acc, name) =>
          ExpressionValue.substitute(acc, name, bindings.getOrElse(name, ExpressionValue.ParameterReference(name, Value.Type)))
        } == queryArg
      }
    }
  }

  private def collectExpressionModuleNames(ev: ExpressionValue): Seq[ModuleName] =
    ev match {
      case ExpressionValue.ConcreteValue(v)          => v.typeFQN.map(_.moduleName).toSeq
      case ExpressionValue.FunctionApplication(t, a) => collectExpressionModuleNames(t) ++ collectExpressionModuleNames(a)
      case _                                         => Seq.empty
    }
}
