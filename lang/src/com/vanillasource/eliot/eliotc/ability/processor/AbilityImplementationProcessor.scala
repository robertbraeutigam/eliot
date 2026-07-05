package com.vanillasource.eliot.eliotc.ability.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.ability.fact.{AbilityImplementation, AbilityImplementationCheck}
import com.vanillasource.eliot.eliotc.ability.util.AbilityMatcher
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.module.fact.{ModuleAbilities, ModuleName, QualifiedName, Qualifier, ValueFQN, WellKnownTypes}
import com.vanillasource.eliot.eliotc.monomorphize.fact.{CompilerMonomorphicValue, GroundValue}
import com.vanillasource.eliot.eliotc.operator.fact.{OperatorResolvedExpression, OperatorResolvedValue}
import com.vanillasource.eliot.eliotc.platform.Platform
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.SingleKeyTypeProcessor
import com.vanillasource.eliot.eliotc.resolve.fact.{AbilityFQN, Qualifier as ResolveQualifier}
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerError

class AbilityImplementationProcessor extends SingleKeyTypeProcessor[AbilityImplementation.Key] with Logging {

  override protected def generateFact(key: AbilityImplementation.Key): CompilerIO[Unit] = {
    val abilityValueFQN                   = key.abilityValueFQN
    val candidateModules: Set[ModuleName] =
      Set(abilityValueFQN.moduleName) ++ key.typeArguments.flatMap(collectModuleNames)

    for {
      abilityFQN  <- abilityValueFQN.name.qualifier match {
                       case Qualifier.Ability(name) => AbilityFQN(abilityValueFQN.moduleName, name).pure[CompilerIO]
                       case other                   => error[CompilerIO](s"expected Ability qualifier, got: $other") >> abort
                     }
      _           <- getFactOrAbort(AbilityImplementationCheck.Key(abilityFQN, key.typeArguments, key.platform))
      candidates  <- candidateModules.toSeq.flatTraverse(module =>
                       findImplementationsInModule(module, abilityValueFQN.name.name, abilityFQN.abilityName, key.platform)
                     )
      matching    <- candidates.flatTraverse(vfqn =>
                        verifyImplementation(vfqn, abilityFQN, key.typeArguments, key.platform)
                      )
      deduplicated = matching.distinctBy(_._1.show)
      _           <- deduplicated match {
                       case Seq((implFQN, implTypeArgs)) =>
                         registerFactIfClear(
                           AbilityImplementation(abilityValueFQN, key.typeArguments, implFQN, implTypeArgs, key.platform)
                         )
                       case Seq()                        =>
                         handleMissingImplementation(abilityValueFQN, abilityFQN, key)
                       case multiple                     =>
                         for {
                           resolved <- getFactOrAbort(OperatorResolvedValue.Key(abilityValueFQN, key.platform))
                           _        <-
                             compilerError(
                               resolved.name.as(
                                 s"Multiple ability implementations found for ability '${abilityFQN.abilityName}' with type arguments ${key.typeArguments
                                     .map(_.show)
                                     .mkString("[", ", ", "]")}."
                               )
                             )
                         } yield ()
                     }
    } yield ()
  }

  private def handleMissingImplementation(
      abilityValueFQN: ValueFQN,
      abilityFQN: AbilityFQN,
      key: AbilityImplementation.Key
  ): CompilerIO[Unit] =
    for {
      resolved <- getFactOrAbort(OperatorResolvedValue.Key(abilityValueFQN, key.platform))
      _        <- resolved.runtime match {
                    case Some(_) =>
                      handleDefaultImplementation(resolved, abilityFQN, key)
                    case None    =>
                      compilerError(
                        resolved.name.as(
                          s"No ability implementation found for ability '${abilityFQN.abilityName}' with type arguments ${key.typeArguments
                              .map(_.show)
                              .mkString("[", ", ", "]")}."
                        )
                      )
                  }
    } yield ()

  private def handleDefaultImplementation(
      abilityResolved: OperatorResolvedValue,
      abilityFQN: AbilityFQN,
      key: AbilityImplementation.Key
  ): CompilerIO[Unit] = {
    val abilityValueFQN  = key.abilityValueFQN
    val candidateModules = Set(abilityValueFQN.moduleName) ++ key.typeArguments.flatMap(collectModuleNames)
    for {
      allImpls <- candidateModules.toSeq.flatTraverse(findAnyImplementationInModule(_, abilityFQN.abilityName, key.platform))
      verified <- allImpls.flatTraverse(verifyImplementation(_, abilityFQN, key.typeArguments, key.platform))
      sibling  <- verified.headOption match {
                    case Some((fqn, _)) => fqn.pure[CompilerIO]
                    case None           =>
                      error[CompilerIO](s"Expected sibling for default '${abilityValueFQN.name.name}' but found none") >>
                        abort[ValueFQN]
                  }
      _        <- registerFactIfClear(
                    AbilityImplementation(
                      key.abilityValueFQN,
                      key.typeArguments,
                      abilityValueFQN,
                      key.typeArguments,
                      key.platform
                    )
                  )
    } yield ()
  }

  private def findAnyImplementationInModule(
      moduleName: ModuleName,
      abilityLocalName: String,
      platform: Platform
  ): CompilerIO[Seq[ValueFQN]] =
    getFactOrAbort(ModuleAbilities.Key(moduleName, platform)).map(_.implementationMethodsOf(abilityLocalName))

  private def findImplementationsInModule(
      moduleName: ModuleName,
      functionName: String,
      abilityLocalName: String,
      platform: Platform
  ): CompilerIO[Seq[ValueFQN]] =
    getFactIfProduced(ModuleAbilities.Key(moduleName, platform)).map {
      case None        => Seq.empty
      case Some(impls) => impls.namedImplementationMethodsOf(abilityLocalName, functionName)
    }

  private def verifyImplementation(
      vfqn: ValueFQN,
      expectedAbilityFQN: AbilityFQN,
      expectedTypeArgs: Seq[GroundValue],
      platform: Platform
  ): CompilerIO[Seq[(ValueFQN, Seq[GroundValue])]] =
    getFactIfProduced(OperatorResolvedValue.Key(vfqn, platform)).flatMap {
      case None           => Seq.empty.pure[CompilerIO]
      case Some(resolved) =>
        resolved.name.value.qualifier match {
          case ResolveQualifier.AbilityImplementation(resolvedAbilityFQN, _)
              if resolvedAbilityFQN == expectedAbilityFQN =>
            for {
              markerVfqn <- markerVfqnFor(vfqn, expectedAbilityFQN.abilityName).pure[CompilerIO]
              marker     <- getFactOrAbort(OperatorResolvedValue.Key(markerVfqn, platform))
              markerSig   = marker.typeStack.as(marker.typeStack.value.signature)
              matched    <- AbilityMatcher.matchImpl(markerSig, expectedTypeArgs)
              result     <- matched match {
                              case None    => Seq.empty[(ValueFQN, Seq[GroundValue])].pure[CompilerIO]
                              case Some(m) => dischargeGuard(vfqn, markerVfqn, markerSig, m)
                            }
            } yield result
          case _ => Seq.empty.pure[CompilerIO]
        }
    }

  /** Filter a matched candidate by its `where` guard (ability-guards Stage 2). The guard rides the marker's
    * return-type slot (§2.3): an unguarded marker's slot is the literal `eliot.lang.Bool::true` reference, kept
    * verbatim without evaluation (and without paying to monomorphize the marker). Any other slot is a real guard,
    * discharged at the concrete match binding by monomorphizing the marker on the **compiler track** — guards are
    * compile-time `Bool` computations (`Eq[Type]` is compiler-pool-only), so they always evaluate there regardless of
    * the queried `platform` — and reading its reduced return. Three outcomes (§3.1): `true` keeps, `false` declines
    * (dropped from candidates so resolution falls through to another instance), `error(msg)` is a hard compile error
    * carrying the author's message.
    *
    * Fail-safe: a real guard is discharged only over faithfully-traced bindings — a `metaToGround`-collapsed binding
    * could compare equal wrongly (§3.1), so an untraced match is an internal error rather than a silent wrong verdict.
    */
  private def dischargeGuard(
      vfqn: ValueFQN,
      markerVfqn: ValueFQN,
      markerSig: Sourced[OperatorResolvedExpression],
      matched: AbilityMatcher.Match
  ): CompilerIO[Seq[(ValueFQN, Seq[GroundValue])]] = {
    val keep = Seq((vfqn, matched.groundArgs))
    if (AbilityMatcher.isUnguarded(markerSig)) keep.pure[CompilerIO]
    else if (!matched.allTraced)
      error[CompilerIO](
        s"Internal: cannot discharge the guard of '${vfqn.name.name}' — a matched type parameter was not faithfully traced."
      ) >> abort[Seq[(ValueFQN, Seq[GroundValue])]]
    else
      getFactOrAbort(CompilerMonomorphicValue.Key(markerVfqn, matched.groundArgs))
        .flatMap(cmv => interpretGuard(cmv.signature.deepReturnType, markerSig, keep))
  }

  /** Interpret a discharged guard's reduced return value (§3.1). Both `Bool` representations are accepted: a body-less
    * `eliot.lang.Bool::true`/`false` reference (a [[GroundValue.Structure]] head) and a native-reduced
    * [[GroundValue.Direct]] boolean. A `Right(bool)` unwraps to its `Bool` payload (a guard using the `{Throw[String]}`
    * channel that succeeded); a `Left(msg)` is a hard rejection carrying the author's message; anything else cannot
    * occur at a concrete use site (a stuck guard) and is an internal error, never a silent accept.
    */
  private def interpretGuard(
      verdict: GroundValue,
      at: Sourced[?],
      keep: Seq[(ValueFQN, Seq[GroundValue])]
  ): CompilerIO[Seq[(ValueFQN, Seq[GroundValue])]] =
    verdict match {
      case GroundValue.Direct(true, _)                                           => keep.pure[CompilerIO]
      case GroundValue.Direct(false, _)                                          => declined
      case GroundValue.Structure(fqn, _, _) if fqn == WellKnownTypes.boolTrueFQN  => keep.pure[CompilerIO]
      case GroundValue.Structure(fqn, _, _) if fqn == WellKnownTypes.boolFalseFQN => declined
      case GroundValue.Structure(fqn, args, _) if fqn == WellKnownTypes.rightFQN  =>
        args.lastOption match {
          case Some(payload) => interpretGuard(payload, at, keep)
          case None          => internalGuardError(at)
        }
      case GroundValue.Structure(fqn, args, _) if fqn == WellKnownTypes.leftFQN   =>
        compilerError(at.as(guardMessage(args.lastOption))) >> abort[Seq[(ValueFQN, Seq[GroundValue])]]
      case _                                                                      => internalGuardError(at)
    }

  private val declined: CompilerIO[Seq[(ValueFQN, Seq[GroundValue])]] =
    Seq.empty[(ValueFQN, Seq[GroundValue])].pure[CompilerIO]

  /** The author message carried by a `Left(msg)` rejection — a literal `String` field. A non-literal (should not occur
    * at a concrete site) falls back to a generic message so the rejection is still reported, never silently dropped.
    */
  private def guardMessage(field: Option[GroundValue]): String =
    field match {
      case Some(GroundValue.Direct(s: String, _)) => s
      case _                                       => "A type guard rejected this use."
    }

  private def internalGuardError(at: Sourced[?]): CompilerIO[Seq[(ValueFQN, Seq[GroundValue])]] =
    error[CompilerIO](
      "Internal: ability guard did not reduce to a concrete `Bool` verdict at a concrete use site."
    ) >> abort[Seq[(ValueFQN, Seq[GroundValue])]]

  private def markerVfqnFor(implVfqn: ValueFQN, abilityName: String): ValueFQN =
    ValueFQN(implVfqn.moduleName, QualifiedName(abilityName, implVfqn.name.qualifier))

  private def collectModuleNames(v: GroundValue): Seq[ModuleName] =
    v match {
      case GroundValue.Structure(typeName, args, _) =>
        args.flatMap(collectModuleNames) :+ typeName.moduleName
      case _                                        => Seq.empty
    }
}
