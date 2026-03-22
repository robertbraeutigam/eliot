package com.vanillasource.eliot.eliotc.implementation.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.core.fact.{QualifiedName, Qualifier}
import com.vanillasource.eliot.eliotc.eval.fact.Value
import com.vanillasource.eliot.eliotc.implementation.fact.AbilityImplementationCheck
import com.vanillasource.eliot.eliotc.matchdesugar.fact.MatchDesugaredExpression
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, UnifiedModuleNames, ValueFQN}
import com.vanillasource.eliot.eliotc.operator.fact.{OperatorResolvedExpression, OperatorResolvedValue}
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.SingleKeyTypeProcessor
import com.vanillasource.eliot.eliotc.resolve.fact.{AbilityFQN, Qualifier as ResolveQualifier}
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerError
import com.vanillasource.eliot.eliotc.implementation.util.{NormalFormEvaluator, QuantifiedType, SymbolicType}

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
                                  s"The type parameter '${typeArguments.map(_.show).mkString(", ")}' does not implement ability '${abilityFQN.abilityName}'."
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

  private case class ResolvedMethod(
      vfqn: ValueFQN,
      name: Sourced[com.vanillasource.eliot.eliotc.resolve.fact.QualifiedName],
      signature: QuantifiedType,
      hasRuntime: Boolean
  )

  private def collectAbilityMethods(abilityFQN: AbilityFQN): CompilerIO[Seq[ResolvedMethod]] =
    getFactOrAbort(UnifiedModuleNames.Key(abilityFQN.moduleName)).flatMap { names =>
      names.names.keys.toSeq
        .collect {
          case qn @ QualifiedName(_, Qualifier.Ability(name)) if name == abilityFQN.abilityName =>
            ValueFQN(abilityFQN.moduleName, qn)
        }
        .traverse(vfqn => toResolvedMethod(vfqn))
    }

  private def collectImplMethods(
      moduleName: ModuleName,
      abilityFQN: AbilityFQN,
      typeArguments: Seq[Value]
  ): CompilerIO[Seq[ResolvedMethod]] =
    getFactOrAbort(UnifiedModuleNames.Key(moduleName)).flatMap { names =>
      names.names.keys.toSeq
        .collect {
          case qn @ QualifiedName(_, Qualifier.AbilityImplementation(abilityNameSrc, _))
              if abilityNameSrc.value == abilityFQN.abilityName =>
            ValueFQN(moduleName, qn)
        }
        .traverse(vfqn => toResolvedMethod(vfqn))
        .flatMap(_.traverseFilter { method =>
          method.name.value.qualifier match {
            case ResolveQualifier.AbilityImplementation(resolvedFQN, paramExprs) if resolvedFQN == abilityFQN =>
              for {
                symbolicParams <- resolveQualifierParams(method.name, paramExprs)
                freeVarNames    = method.signature.typeParams.map(_._1).toSet
                symbolicArgs    = typeArguments.map(SymbolicType.fromValue)
              } yield if (implMatchesQuery(symbolicParams, freeVarNames, symbolicArgs)) Some(method) else None
            case _ => None.pure[CompilerIO]
          }
        })
    }

  private def toResolvedMethod(vfqn: ValueFQN): CompilerIO[ResolvedMethod] =
    for {
      resolved      <- getFactOrAbort(OperatorResolvedValue.Key(vfqn))
      signatureType <- NormalFormEvaluator.evaluate(
                         resolved.typeStack.as(resolved.typeStack.value.signature)
                       )
    } yield ResolvedMethod(
      vfqn,
      resolved.name,
      QuantifiedType.fromSymbolicType(signatureType),
      resolved.runtime.isDefined
    )

  private def checkCompleteness(
      abilityMethods: Seq[ResolvedMethod],
      implMethods: Seq[ResolvedMethod]
  ): CompilerIO[Unit] = {
    val implMethodNames = implMethods.map(_.vfqn.name.name).toSet
    val abstractMethods = abilityMethods.filter(!_.hasRuntime)
    abstractMethods
      .filterNot(m => implMethodNames.contains(m.vfqn.name.name))
      .traverse_ { m =>
        implMethods.headOption match {
          case Some(implHead) =>
            val implSource = implHead.vfqn.name.qualifier.asInstanceOf[Qualifier.AbilityImplementation].name
            compilerError(implSource.as(s"Ability implementation is missing method '${m.vfqn.name.name}'."))
          case None           =>
            compilerError(m.name.map(qn => s"Ability implementation is missing method '${qn.name}'."))
        }
      }
  }

  private def checkNoExtras(
      abilityMethods: Seq[ResolvedMethod],
      implMethods: Seq[ResolvedMethod]
  ): CompilerIO[Unit] = {
    val abilityMethodNames = abilityMethods.map(_.vfqn.name.name).toSet
    implMethods
      .filterNot(m => abilityMethodNames.contains(m.vfqn.name.name))
      .traverse_(m =>
        compilerError(
          m.name.map(qn => s"Method not defined in ability.")
        )
      )
  }

  private def checkSignatures(
      abilityMethods: Seq[ResolvedMethod],
      implMethods: Seq[ResolvedMethod]
  ): CompilerIO[Unit] = {
    val implByName = implMethods.map(m => m.vfqn.name.name -> m).toMap
    abilityMethods
      .flatMap(abstractMethod => implByName.get(abstractMethod.vfqn.name.name).map(abstractMethod -> _))
      .traverse_ { case (abstractMethod, implMethod) =>
        val abilityTypeParamNames = abstractMethod.signature.typeParams
        val implParams            = implMethod.name.value.qualifier match {
          case ResolveQualifier.AbilityImplementation(_, params) => params
          case _                                                  => Seq.empty
        }
        // We need to evaluate the impl params to SymbolicType for substitution
        for {
          evaluatedImplParams <- resolveQualifierParams(implMethod.name, implParams)
          patternBindings      = abilityTypeParamNames.map(_._1).zip(evaluatedImplParams).toMap
          expectedImplSig      = patternBindings.foldLeft(abstractMethod.signature.body) { case (acc, (name, param)) =>
                                   SymbolicType.substitute(acc, name, param)
                                 }
          actualImplSig        = implMethod.signature.body
          unificationVarBindings =
            SymbolicType.matchTypes(expectedImplSig, actualImplSig, isUnificationVarName)
          resolvedExpectedSig    = unificationVarBindings.foldLeft(expectedImplSig) { case (acc, (name, value)) =>
                                     SymbolicType.substitute(acc, name, value)
                                   }
          _                     <- if (resolvedExpectedSig != actualImplSig)
                                     compilerError(
                                       implMethod.name.map(qn => s"Signature of implementation does not match the ability definition.")
                                     )
                                   else ().pure[CompilerIO]
        } yield ()
      }
  }

  private def isUnificationVarName(name: String): Boolean = name.endsWith("$")

  private def resolveQualifierParams(
      name: Sourced[?],
      expressions: Seq[com.vanillasource.eliot.eliotc.resolve.fact.Expression]
  ): CompilerIO[Seq[SymbolicType]] =
    expressions.traverse { expression =>
      NormalFormEvaluator.evaluate(
        name.as(OperatorResolvedExpression.fromExpression(MatchDesugaredExpression.fromExpression(expression)))
      )
    }

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

  private def collectModuleNames(v: Value): Seq[ModuleName] =
    v match {
      case Value.Structure(fields, _) =>
        fields.values.toSeq.flatMap(collectModuleNames) ++
          fields.get("$typeName").toSeq.collect { case Value.Direct(vfqn: ValueFQN, _) => vfqn.moduleName }
      case _                          => Seq.empty
    }
}
