package com.vanillasource.eliot.eliotc.ability.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.ability.fact.AbilityImplementation.Resolution
import com.vanillasource.eliot.eliotc.ability.fact.{AbilityImplementation, AbilityImplementationCheck}
import com.vanillasource.eliot.eliotc.ability.util.AbilityMatcher
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.module.fact.{ModuleAbilities, ModuleName, QualifiedName, Qualifier, ValueFQN, WellKnownTypes}
import com.vanillasource.eliot.eliotc.monomorphize.check.GuardChannel
import com.vanillasource.eliot.eliotc.monomorphize.fact.{CompilerMonomorphicValue, GroundValue, MonomorphicValue}
import com.vanillasource.eliot.eliotc.operator.fact.{OperatorResolvedExpression, OperatorResolvedValue}
import com.vanillasource.eliot.eliotc.platform.Platform
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.SingleKeyTypeProcessor
import com.vanillasource.eliot.eliotc.resolve.fact.{AbilityFQN, Qualifier as ResolveQualifier}
import com.vanillasource.eliot.eliotc.source.content.Sourced

/** Resolves the ability implementation for one `(ability, type arguments, platform)` query: collects the candidate
  * implementations, matches each pattern, discharges each `where` guard, and registers the aggregate
  * [[Resolution]] outcome on the [[AbilityImplementation]] fact. This processor **reports no resolution errors** —
  * whether a failed resolution is a compile error, and at what source position, is the demander's decision (the
  * checker's saturation pass reports a failed *demand* at its use-site reference; the checker-inserted probes like
  * `Coerce` treat non-applicability as a normal decline). It still aborts on internal errors, which leave the fact
  * absent.
  */
class AbilityImplementationProcessor extends SingleKeyTypeProcessor[AbilityImplementation.Key] with Logging {

  /** How one candidate implementation fared at the queried type arguments: kept (pattern matched, guard admitted),
    * declined (no structural match, or the guard reduced to `false`), or rejected (the guard `error(msg)`-ed —
    * ability-guards §3.1).
    */
  private enum Verdict {
    case Keep(implFQN: ValueFQN, implTypeArgs: Seq[GroundValue])
    case Decline
    case Reject(message: String)
  }

  private val decline: CompilerIO[Verdict] = (Verdict.Decline: Verdict).pure[CompilerIO]

  override protected def generateFact(key: AbilityImplementation.Key): CompilerIO[Unit] = {
    val abilityValueFQN                   = key.abilityValueFQN
    val candidateModules: Set[ModuleName] =
      Set(abilityValueFQN.moduleName) ++ key.typeArguments.flatMap(collectModuleNames)

    for {
      abilityFQN <- abilityValueFQN.name.qualifier match {
                      case Qualifier.Ability(name) => AbilityFQN(abilityValueFQN.moduleName, name).pure[CompilerIO]
                      case other                   => error[CompilerIO](s"expected Ability qualifier, got: $other") >> abort
                    }
      _          <- getFactOrAbort(AbilityImplementationCheck.Key(abilityFQN, key.typeArguments, key.platform))
      candidates <- candidateModules.toSeq.flatTraverse(module =>
                      findImplementationsInModule(module, abilityValueFQN.name.name, abilityFQN.abilityName, key.platform)
                    )
      verdicts   <- candidates.traverse(verifyImplementation(_, abilityFQN, key.typeArguments, key.platform))
      resolution <- interpretVerdicts(verdicts, abilityValueFQN, abilityFQN, key)
      _          <- registerFactIfClear(AbilityImplementation(abilityValueFQN, key.typeArguments, resolution, key.platform))
    } yield ()
  }

  /** Aggregate the per-candidate verdicts into the query's [[Resolution]] (§3: exactly-one-survivor). A hard rejection
    * dominates — the instance author explicitly ruled this instantiation out, with a message. Otherwise the kept
    * candidates decide: exactly one resolves, none may still fall back to the ability method's own default
    * implementation, several is the use-site ambiguity.
    */
  private def interpretVerdicts(
      verdicts: Seq[Verdict],
      abilityValueFQN: ValueFQN,
      abilityFQN: AbilityFQN,
      key: AbilityImplementation.Key
  ): CompilerIO[Resolution] = {
    val rejections = verdicts.collect { case Verdict.Reject(message) => message }
    val kept       = verdicts.collect { case Verdict.Keep(implFQN, implTypeArgs) => (implFQN, implTypeArgs) }
      .distinctBy(_._1.show)
    if (rejections.nonEmpty) (Resolution.Rejected(rejections): Resolution).pure[CompilerIO]
    else
      kept match {
        case Seq((implFQN, implTypeArgs)) => (Resolution.Resolved(implFQN, implTypeArgs): Resolution).pure[CompilerIO]
        case Seq()                        => missingResolution(abilityValueFQN, abilityFQN, key)
        case _                            => (Resolution.Ambiguous: Resolution).pure[CompilerIO]
      }
  }

  /** No named implementation of the queried method applies. If the ability method carries a default body, the type
    * still implements the ability whenever *some* implementation block applies at these type arguments — the default
    * then resolves to itself. Otherwise the outcome is [[Resolution.NoImplementation]], for the demander to interpret.
    * (This also retires the former internal-error corner where a default's every sibling declined its guard: that is
    * now an ordinary `NoImplementation`.)
    */
  private def missingResolution(
      abilityValueFQN: ValueFQN,
      abilityFQN: AbilityFQN,
      key: AbilityImplementation.Key
  ): CompilerIO[Resolution] =
    for {
      resolved   <- getFactOrAbort(OperatorResolvedValue.Key(abilityValueFQN, key.platform))
      resolution <- resolved.runtime match {
                      case Some(_) => defaultResolution(abilityFQN, key)
                      case None    => (Resolution.NoImplementation: Resolution).pure[CompilerIO]
                    }
    } yield resolution

  /** The default-implementation arm of [[missingResolution]]: the ability method has a body of its own, so it resolves
    * to itself if any implementation block of this ability applies at the queried type arguments (evidence the type
    * implements the ability at all — the block need not name this method).
    */
  private def defaultResolution(
      abilityFQN: AbilityFQN,
      key: AbilityImplementation.Key
  ): CompilerIO[Resolution] = {
    val candidateModules = Set(key.abilityValueFQN.moduleName) ++ key.typeArguments.flatMap(collectModuleNames)
    for {
      allImpls <- candidateModules.toSeq.flatTraverse(
                    findAnyImplementationInModule(_, abilityFQN.abilityName, key.platform)
                  )
      verdicts <- allImpls.traverse(verifyImplementation(_, abilityFQN, key.typeArguments, key.platform))
    } yield {
      val rejections = verdicts.collect { case Verdict.Reject(message) => message }
      if (rejections.nonEmpty) Resolution.Rejected(rejections)
      else if (verdicts.exists { case _: Verdict.Keep => true; case _ => false })
        Resolution.Resolved(key.abilityValueFQN, key.typeArguments)
      else Resolution.NoImplementation
    }
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
  ): CompilerIO[Verdict] =
    getFactIfProduced(OperatorResolvedValue.Key(vfqn, platform)).flatMap {
      case None           => decline
      case Some(resolved) =>
        resolved.name.value.qualifier match {
          case ResolveQualifier.AbilityImplementation(resolvedAbilityFQN, _)
              if resolvedAbilityFQN == expectedAbilityFQN =>
            for {
              markerVfqn <- markerVfqnFor(vfqn, expectedAbilityFQN.abilityName).pure[CompilerIO]
              marker     <- getFactOrAbort(OperatorResolvedValue.Key(markerVfqn, platform))
              markerSig   = marker.typeStack.as(marker.typeStack.value.signature)
              matched    <- AbilityMatcher.matchImpl(markerSig, expectedTypeArgs)
              verdict    <- matched match {
                              case None    => decline
                              case Some(m) => dischargeGuard(vfqn, markerVfqn, markerSig, m, platform)
                            }
            } yield verdict
          case _ => decline
        }
    }

  /** Judge a matched candidate by its `where` guard (ability-guards Stage 2). The guard rides the marker's return-type
    * slot (§2.3): an unguarded marker's slot is the literal `eliot.lang.Bool::true` reference, kept verbatim without
    * evaluation (and without paying to monomorphize the marker). Any other slot is a real guard, discharged at the
    * concrete match binding by monomorphizing the marker and reading its reduced return. Three verdicts (§3.1): `true`
    * keeps, `false` declines (resolution falls through to another instance), `error(msg)` rejects with the author's
    * message — carried on the [[Resolution.Rejected]] outcome for the demanding use site to report.
    *
    * The marker is monomorphized on the **queried `platform`'s track** — the platform where the implementation (and its
    * synthesized marker) is declared, which is the only pool the marker exists in. The guard is still a compile-time
    * `Bool` computation, but it reduces on either track: its `Eq[Type]` instance lives in the base layer (on both
    * source paths) and its `typeEquals` leaf is a platform-agnostic native. So a runtime-layer guarded instance (the
    * `Throw` self-lift, `where E1 != E2`) is discharged on the runtime track — where its marker lives — while a
    * compiler-pool guarded instance is discharged on the compiler track.
    *
    * Fail-safe: a real guard is discharged only over faithfully-traced bindings — a `metaToGround`-collapsed binding
    * could compare equal wrongly (§3.1), so an untraced match is an internal error rather than a silent wrong verdict.
    */
  private def dischargeGuard(
      vfqn: ValueFQN,
      markerVfqn: ValueFQN,
      markerSig: Sourced[OperatorResolvedExpression],
      matched: AbilityMatcher.Match,
      platform: Platform
  ): CompilerIO[Verdict] = {
    val keep: Verdict = Verdict.Keep(vfqn, matched.groundArgs)
    if (AbilityMatcher.isUnguarded(markerSig)) keep.pure[CompilerIO]
    else if (!matched.allTraced)
      error[CompilerIO](
        s"Internal: cannot discharge the guard of '${vfqn.name.name}' — a matched type parameter was not faithfully traced."
      ) >> abort[Verdict]
    else readGuardVerdict(markerVfqn, matched.groundArgs, platform).flatMap(interpretGuard(_, keep))
  }

  /** Monomorphize the marker at its matched ground arguments on the queried platform's track and read its reduced
    * return-slot guard. The two tracks are distinct fact types ([[MonomorphicValue]] / [[CompilerMonomorphicValue]]),
    * both carrying a ground `signature`; the guard verdict is that signature's deep return type.
    */
  private def readGuardVerdict(
      markerVfqn: ValueFQN,
      groundArgs: Seq[GroundValue],
      platform: Platform
  ): CompilerIO[GroundValue] =
    platform match {
      case Platform.Compiler =>
        getFactOrAbort(CompilerMonomorphicValue.Key(markerVfqn, groundArgs)).map(_.signature.deepReturnType)
      case Platform.Runtime  =>
        getFactOrAbort(MonomorphicValue.Key(markerVfqn, groundArgs)).map(_.signature.deepReturnType)
    }

  /** Interpret a discharged guard's reduced return value (§3.1) into a [[Verdict]]. Both `Bool` representations are
    * accepted: a body-less `eliot.lang.Bool::true`/`false` reference (a [[GroundValue.Structure]] head) and a
    * native-reduced [[GroundValue.Direct]] boolean. A `Right(bool)` unwraps to its `Bool` payload (a guard using the
    * `{Throw[String]}` channel that succeeded); a `Left(msg)` is a hard rejection carrying the author's message;
    * anything else cannot occur at a concrete use site (a stuck guard) and is an internal error, never a silent accept.
    * (The `Right`/`Left` payload convention and the rejection fallback are the shared [[GuardChannel]] protocol.)
    */
  private def interpretGuard(verdict: GroundValue, keep: Verdict): CompilerIO[Verdict] =
    verdict match {
      case GroundValue.Direct(true, _)                                            => keep.pure[CompilerIO]
      case GroundValue.Direct(false, _)                                           => decline
      case GroundValue.Structure(fqn, _, _) if fqn == WellKnownTypes.boolTrueFQN  => keep.pure[CompilerIO]
      case GroundValue.Structure(fqn, _, _) if fqn == WellKnownTypes.boolFalseFQN => decline
      case GroundValue.Structure(fqn, args, _) if fqn == WellKnownTypes.rightFQN  =>
        GuardChannel.payload(args) match {
          case Some(payload) => interpretGuard(payload, keep)
          case None          => internalGuardError
        }
      case GroundValue.Structure(fqn, args, _) if fqn == WellKnownTypes.leftFQN   =>
        (Verdict.Reject(rejectionText(GuardChannel.payload(args))): Verdict).pure[CompilerIO]
      case _                                                                      => internalGuardError
    }

  /** The author message carried by a `Left(msg)` rejection — a literal `String` field. A non-literal (should not occur
    * at a concrete site) falls back to the shared generic message so the rejection is still reported, never silently
    * dropped.
    */
  private def rejectionText(field: Option[GroundValue]): String =
    field match {
      case Some(GroundValue.Direct(s: String, _)) => s
      case _                                      => GuardChannel.fallbackRejectionMessage
    }

  private def internalGuardError: CompilerIO[Verdict] =
    error[CompilerIO](
      "Internal: ability guard did not reduce to a concrete `Bool` verdict at a concrete use site."
    ) >> abort[Verdict]

  private def markerVfqnFor(implVfqn: ValueFQN, abilityName: String): ValueFQN =
    ValueFQN(implVfqn.moduleName, QualifiedName(abilityName, implVfqn.name.qualifier))

  private def collectModuleNames(v: GroundValue): Seq[ModuleName] =
    v match {
      case GroundValue.Structure(typeName, args, _) =>
        args.flatMap(collectModuleNames) :+ typeName.moduleName
      case _                                        => Seq.empty
    }
}
