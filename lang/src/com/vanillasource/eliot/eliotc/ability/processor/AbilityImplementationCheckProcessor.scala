package com.vanillasource.eliot.eliotc.ability.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.ability.fact.{AbilityImplementationCheck, ModuleAbilityOverlapCheck}
import com.vanillasource.eliot.eliotc.ability.util.AbilityMatcher
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, QualifiedName, Qualifier, UnifiedModuleNames, ValueFQN}
import com.vanillasource.eliot.eliotc.monomorphize.fact.GroundValue
import com.vanillasource.eliot.eliotc.operator.fact.{OperatorResolvedExpression, OperatorResolvedValue}
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.SingleKeyTypeProcessor
import com.vanillasource.eliot.eliotc.resolve.fact.{AbilityFQN, Qualifier as ResolveQualifier}
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerError

class AbilityImplementationCheckProcessor extends SingleKeyTypeProcessor[AbilityImplementationCheck.Key] {

  override protected def generateFact(key: AbilityImplementationCheck.Key): CompilerIO[Unit] = {
    val abilityFQN                        = key.abilityFQN
    val typeArguments                     = key.typeArguments
    val candidateModules: Set[ModuleName] =
      Set(abilityFQN.moduleName) ++ typeArguments.flatMap(collectModuleNames)

    for {
      // Trigger the per-module overlap check eagerly for every module that could contain an impl of
      // this ability. The fact is cached per (module, abilityName), so the actual overlap scan runs
      // at most once per pair across the whole compilation regardless of how many call sites hit it.
      // `getFact` (not `getFactOrAbort`) — if no overlap processor is registered in the current
      // pipeline (some unit tests exclude it), skip the check silently rather than abort.
      _              <- candidateModules.toSeq.traverse_(m =>
                          getFact(ModuleAbilityOverlapCheck.Key(m, abilityFQN.abilityName)).void
                        )
      abilityMethods <- collectAbilityMethods(abilityFQN)
      implMethods    <- candidateModules.toSeq.flatTraverse(collectImplMethods(_, abilityFQN, typeArguments))
      _              <- implMethods match {
                          case Nil =>
                            // Point the error at the ability marker (the synthetic method whose local name
                            // equals the ability name) when available, otherwise at any ability method.
                            val errorSource =
                              abilityMethods.find(_.vfqn.name.name == abilityFQN.abilityName)
                                .orElse(abilityMethods.headOption)
                            errorSource.traverse_(m =>
                              compilerError(
                                m.name.as(
                                  s"The type parameter '${typeArguments.map(_.show).mkString(", ")}' does not implement ability '${abilityFQN.abilityName}'."
                                )
                              )
                            )
                          case _   =>
                            checkCompleteness(abilityMethods, implMethods) >>
                              checkNoExtras(abilityMethods, implMethods) >>
                              checkSignatures(abilityFQN, abilityMethods, implMethods)
                        }
      _              <- registerFactIfClear(AbilityImplementationCheck(abilityFQN, typeArguments))
    } yield ()
  }

  private case class ResolvedMethod(
      vfqn: ValueFQN,
      name: Sourced[com.vanillasource.eliot.eliotc.resolve.fact.QualifiedName],
      methodSig: Sourced[OperatorResolvedExpression],
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
      typeArguments: Seq[GroundValue]
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
            case ResolveQualifier.AbilityImplementation(resolvedFQN, _) if resolvedFQN == abilityFQN =>
              for {
                markerSig <- loadMarkerSignature(method.vfqn, abilityFQN.abilityName)
                matched   <- AbilityMatcher.matchImpl(markerSig, typeArguments)
              } yield matched.map(_ => method)
            case _                                                                                   =>
              None.pure[CompilerIO]
          }
        })
    }

  private def toResolvedMethod(vfqn: ValueFQN): CompilerIO[ResolvedMethod] =
    getFactOrAbort(OperatorResolvedValue.Key(vfqn)).map(resolved =>
      ResolvedMethod(
        vfqn,
        resolved.name,
        resolved.typeStack.as(resolved.typeStack.value.signature),
        resolved.runtime.isDefined
      )
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
      abilityFQN: AbilityFQN,
      abilityMethods: Seq[ResolvedMethod],
      implMethods: Seq[ResolvedMethod]
  ): CompilerIO[Unit] = {
    val implByName = implMethods.map(m => m.vfqn.name.name -> m).toMap
    abilityMethods
      .flatMap(abstractMethod => implByName.get(abstractMethod.vfqn.name.name).map(abstractMethod -> _))
      .traverse_ { case (abstractMethod, implMethod) =>
        for {
          implMarkerSig <- loadMarkerSignature(implMethod.vfqn, abilityFQN.abilityName)
          compatible    <- AbilityMatcher.signaturesCompatible(
                             abstractMethod.methodSig,
                             implMethod.methodSig,
                             implMarkerSig
                           )
          _             <- if (!compatible)
                             compilerError(
                               implMethod.name.map(_ => s"Signature of implementation does not match the ability definition.")
                             )
                           else ().pure[CompilerIO]
        } yield ()
      }
  }

  private def loadMarkerSignature(
      methodVfqn: ValueFQN,
      abilityName: String
  ): CompilerIO[Sourced[OperatorResolvedExpression]] = {
    val markerVfqn = ValueFQN(
      methodVfqn.moduleName,
      QualifiedName(abilityName, methodVfqn.name.qualifier)
    )
    getFactOrAbort(OperatorResolvedValue.Key(markerVfqn)).map(r => r.typeStack.as(r.typeStack.value.signature))
  }

  private def collectModuleNames(v: GroundValue): Seq[ModuleName] =
    v match {
      case GroundValue.Structure(typeName, args, _) =>
        args.flatMap(collectModuleNames) :+ typeName.moduleName
      case _                                        => Seq.empty
    }
}
