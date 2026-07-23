package com.vanillasource.eliot.eliotc.ability.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.ability.fact.{AbilityImplementationCheck, ModuleAbilityOverlapCheck}
import com.vanillasource.eliot.eliotc.ability.util.AbilityMatcher
import com.vanillasource.eliot.eliotc.effect.processor.{EffectCarriers, EffectMachinery}
import com.vanillasource.eliot.eliotc.module.fact.{ModuleAbilities, ModuleName, QualifiedName, Qualifier, ValueFQN}
import com.vanillasource.eliot.eliotc.monomorphize.fact.GroundValue
import com.vanillasource.eliot.eliotc.operator.fact.{OperatorResolvedExpression, OperatorResolvedValue}
import com.vanillasource.eliot.eliotc.platform.Platform
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.SingleKeyTypeProcessor
import com.vanillasource.eliot.eliotc.resolve.fact.{AbilityFQN, Qualifier as ResolveQualifier}
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerError

class AbilityImplementationCheckProcessor(effectChannel: Boolean = false)
    extends SingleKeyTypeProcessor[AbilityImplementationCheck.Key] {

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
                          getFactIfProduced(ModuleAbilityOverlapCheck.Key(m, abilityFQN.abilityName, key.platform)).void
                        )
      abilityMethods <- collectAbilityMethods(abilityFQN, key.platform)
      implMethods    <- candidateModules.toSeq.flatTraverse(collectImplMethods(_, abilityFQN, typeArguments, key.platform))
      _              <- implMethods match {
                          case Nil =>
                            // No implementation matches structurally — nothing to signature-check, and not an error
                            // *here*: `AbilityImplementationProcessor` registers the `NoImplementation` outcome, and
                            // it is the demander that decides whether that is a compile error (a demanded capability,
                            // reported at its use site) or a normal decline (a checker-inserted probe like `Coerce`).
                            ().pure[CompilerIO]
                          case _   =>
                            checkCompleteness(abilityMethods, implMethods) >>
                              checkNoExtras(abilityMethods, implMethods) >>
                              // Effects-as-channel Phase 3: under `--effect-channel` a *user* effect ability is
                              // carrier-erased (`op: F[Unit]` ⤳ `op: Unit`) but its instances are NOT (`op: F[Unit]`),
                              // so the character-exact ability↔impl conformance would spuriously fail even though they
                              // conform by construction (the impl is the carrier-headed runtime truth, the erased
                              // ability its effect-blind view). Skip the lexical signature check for those; every other
                              // ability (first-order, and the carrier machinery which keeps its carrier) is checked as
                              // usual, and the impl's own monomorphization still type-checks its body at the carrier.
                              (if (effectChannel && isUserEffectAbility(abilityFQN, abilityMethods)) ().pure[CompilerIO]
                               else checkSignatures(abilityFQN, abilityMethods, implMethods, key.platform))
                        }
      _              <- registerFactIfClear(AbilityImplementationCheck(abilityFQN, typeArguments, key.platform))
    } yield ()
  }

  private case class ResolvedMethod(
      vfqn: ValueFQN,
      name: Sourced[com.vanillasource.eliot.eliotc.resolve.fact.QualifiedName],
      methodSig: Sourced[OperatorResolvedExpression],
      hasRuntime: Boolean
  )

  private def collectAbilityMethods(abilityFQN: AbilityFQN, platform: Platform): CompilerIO[Seq[ResolvedMethod]] =
    getFactOrAbort(ModuleAbilities.Key(abilityFQN.moduleName, platform)).flatMap { abilities =>
      abilities.declaredMethodsOf(abilityFQN.abilityName).traverse(vfqn => toResolvedMethod(vfqn, platform))
    }

  private def collectImplMethods(
      moduleName: ModuleName,
      abilityFQN: AbilityFQN,
      typeArguments: Seq[GroundValue],
      platform: Platform
  ): CompilerIO[Seq[ResolvedMethod]] =
    getFactOrAbort(ModuleAbilities.Key(moduleName, platform)).flatMap { impls =>
      impls
        .implementationMethodsOf(abilityFQN.abilityName)
        .traverse(vfqn => toResolvedMethod(vfqn, platform))
        .flatMap(_.traverseFilter { method =>
          method.name.value.qualifier match {
            case ResolveQualifier.AbilityImplementation(resolvedFQN, _) if resolvedFQN == abilityFQN =>
              for {
                markerSig <- loadMarkerSignature(method.vfqn, abilityFQN.abilityName, platform)
                matched   <- AbilityMatcher.matchImpl(markerSig, typeArguments)
              } yield matched.map(_ => method)
            case _                                                                                   =>
              None.pure[CompilerIO]
          }
        })
    }

  private def toResolvedMethod(vfqn: ValueFQN, platform: Platform): CompilerIO[ResolvedMethod] =
    getFactOrAbort(OperatorResolvedValue.Key(vfqn, platform)).map(resolved =>
      ResolvedMethod(
        vfqn,
        resolved.name,
        resolved.signature,
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
            compilerError(implHead.name.as(s"Ability implementation is missing method '${m.vfqn.name.name}'."))
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

  /** Whether `abilityFQN` is a *user* effect ability — a carrier-parametric ability that is not the carrier machinery.
    * Recognised, as elsewhere under the flag, by the ability *marker* method (local name == ability name) carrying a
    * higher-kinded binder; the machinery (`Effect`/`Suspend`) is excluded because its methods are not carrier-erased.
    */
  private def isUserEffectAbility(abilityFQN: AbilityFQN, abilityMethods: Seq[ResolvedMethod]): Boolean =
    !EffectMachinery.isMachineryAbility(abilityFQN.abilityName) &&
      abilityMethods.exists(m =>
        m.vfqn.name.name == abilityFQN.abilityName &&
          EffectCarriers.carrierBinders(OperatorResolvedExpression.SignatureView.of(m.methodSig)).nonEmpty
      )

  private def checkSignatures(
      abilityFQN: AbilityFQN,
      abilityMethods: Seq[ResolvedMethod],
      implMethods: Seq[ResolvedMethod],
      platform: Platform
  ): CompilerIO[Unit] = {
    val implByName = implMethods.map(m => m.vfqn.name.name -> m).toMap
    abilityMethods
      // Skip the synthetic marker method (local name == ability name): it is a pattern vessel, not a real ability
      // method. Its return-type slot now carries the implementation guard (ability-guards §2.3), which is not part of
      // the method contract, so comparing the impl marker's signature against the ability marker's would spuriously
      // unify the guard against the ability marker's own return. The marker's pattern is already validated at this
      // same use site by `matchImpl` in `collectImplMethods`.
      .filterNot(_.vfqn.name.name == abilityFQN.abilityName)
      .flatMap(abstractMethod => implByName.get(abstractMethod.vfqn.name.name).map(abstractMethod -> _))
      .traverse_ { case (abstractMethod, implMethod) =>
        for {
          implMarkerSig <- loadMarkerSignature(implMethod.vfqn, abilityFQN.abilityName, platform)
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
      abilityName: String,
      platform: Platform
  ): CompilerIO[Sourced[OperatorResolvedExpression]] = {
    val markerVfqn = ValueFQN(
      methodVfqn.moduleName,
      QualifiedName(abilityName, methodVfqn.name.qualifier)
    )
    getFactOrAbort(OperatorResolvedValue.Key(markerVfqn, platform)).map(r => r.signature)
  }

  private def collectModuleNames(v: GroundValue): Seq[ModuleName] =
    v match {
      case GroundValue.Structure(typeName, args, _) =>
        args.flatMap(collectModuleNames) :+ typeName.moduleName
      case _                                        => Seq.empty
    }
}
