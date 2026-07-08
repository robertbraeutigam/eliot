package com.vanillasource.eliot.eliotc.effect.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedExpression
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedExpression.{
  FunctionApplication,
  FunctionLiteral,
  IntegerLiteral,
  ParameterReference,
  StringLiteral,
  ValueReference
}
import com.vanillasource.eliot.eliotc.platform.Platform
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.resolve.fact.AbilityFQN
import com.vanillasource.eliot.eliotc.source.content.Sourced

/** The effect *accounting* walk — the analysis half of the former direct-style desugarer, kept after the auto-lift
  * itself moved into the NbE checker (docs/effect-lift-in-checker.md, Step 4). Collects, for one value's body:
  *
  *   - `usedEffects` — the user-facing effect abilities the body performs: per application spine, the callee's
  *     [[CalleeSignatures.CalleeInfo.effectAbilities]] gated on
  *     [[CalleeSignatures.CalleeInfo.resultEffectful]] (a partially-applied effectful callee has not yet performed its
  *     effect), *minus* the effects the callee discharges (`dischargedEffects`, discharge-aware accounting); read by the
  *     declared-effects subset check ([[DeclaredEffectChecker]]) and the `Inf` propagation it implies.
  *   - `effectful` — whether the body's static result is effectful (carrier-headed): a callee spine via
  *     `resultEffectful`, a parameter reference via its declared type being headed by one of the value's own carrier
  *     binders; read by the "declared pure but performs effects" fail-safe.
  *   - `survivingParamEffects` — the *provenance* channel (discharge inference, Step 3): the effects that entered via
  *     the value's own carrier-typed parameters and still reach this subtree's result *undischarged*. A parameter
  *     reference seeds its carrier's declared effects; a discharger removes the ones it discharges from the arg-union.
  *     An effect survives iff some undischarged occurrence remains — so
  *     [[EffectDischargeSummaryProcessor]] infers a value's discharge as its parameters' effects *minus* those that
  *     survived its body.
  *
  * Everything here is *signature-derivable* and definition-local — sequencing never changes *which* effects a body
  * performs, so this accounting is independent of the checker's bind decisions. No bind-position, branch-position or
  * machinery-sniffing logic survives from the desugarer; the one carrier-annotation read kept is the
  * immediately-applied-lambda (`let`) storage rule, mirroring the checker's own: an effectful bound value makes the
  * `let` effectful unless the binder is annotated carrier-typed (deliberate storage).
  */
class EffectUsageCollector(calleeSignatures: CalleeSignatures) {
  import EffectUsageCollector.*

  /** Collect the usage of `expr`. `env` maps in-scope value parameters to their declared types; `carrierEffects` maps
    * the current value's higher-kinded, ability-constrained carrier binder names to the effects each declares (its
    * key-set is the value's carrier binders).
    */
  def collect(
      expr: Sourced[OperatorResolvedExpression],
      env: Map[String, OperatorResolvedExpression],
      carrierEffects: Map[String, Set[AbilityFQN]]
  )(using Platform): CompilerIO[Usage] =
    expr.value match {
      case _: IntegerLiteral | _: StringLiteral       => Usage.pure.pure[CompilerIO]
      case ParameterReference(name)                   =>
        // A parameter headed by an ability-constrained carrier binder carries that binder's declared effects; they
        // become surviving occurrences tagged to this parameter's origin (its effects, at ability granularity).
        val head    = env.get(name.value).flatMap(EffectCarriers.carrierHead(_, carrierEffects.keySet))
        val effects = head.flatMap(carrierEffects.get).getOrElse(Set.empty)
        Usage(head.isDefined, Set.empty, effects).pure[CompilerIO]
      case FunctionLiteral(name, _, lambdaBody)       =>
        // The lambda *value* is pure (a function); its body's performed effects still propagate.
        collect(lambdaBody, env - name.value, carrierEffects).map(_.copy(effectful = false))
      case _: ValueReference | _: FunctionApplication =>
        collectApplication(expr, env, carrierEffects)
    }

  private def collectApplication(
      expr: Sourced[OperatorResolvedExpression],
      env: Map[String, OperatorResolvedExpression],
      carrierEffects: Map[String, Set[AbilityFQN]]
  )(using platform: Platform): CompilerIO[Usage] = {
    val (head, args) = OperatorResolvedExpression.spine(expr.value)
    head match {
      case ValueReference(fqn, _)                                      =>
        for {
          info     <- calleeSignatures.infoFor(expr.as(fqn.value), platform)
          argUsage <- args.foldMapM(collect(_, env, carrierEffects))
          effectful = info.resultEffectful(args.size)
          // Shallow discharge subtraction (Step 2): a callee that discharges effect `E` (a discharger primitive such
          // as `else`/`catch`/`runStateToPair`, or a user function inferred to discharge via Step 3) removes `E` from
          // the union of its arguments' effects — the discharged effect lived and died inside the argument
          // sub-expression it consumes. Subtracting from the whole arg-union is sound: the non-computation arguments
          // (`fallback`, `initial`, `onError`) ride the inner carrier and cannot carry the discharged ability. The
          // callee's own performed effects (`effectAbilities`) are added back untouched — a discharger that *also*
          // performs a fresh effect (whose result carries it) still propagates it via that channel.
          used      = (argUsage.usedEffects -- info.dischargedEffects) ++ (if (effectful) info.effectAbilities else Set.empty)
          // Provenance: the parameter-originated effects still reaching this call's result — the arg-union's survivors
          // minus what this callee discharges (a discharger consumes those occurrences).
          surviving = argUsage.survivingParamEffects -- info.dischargedEffects
        } yield Usage(effectful, used, surviving)
      case FunctionLiteral(param, paramType, body) if args.sizeIs == 1 =>
        // An immediately-applied lambda `(x -> body)(arg)` — a `let`. An effectful bound value makes the let
        // effectful (the checker sequences it) unless the binder is annotated carrier-typed (deliberate storage). The
        // bound value's surviving param-effects are unioned in conservatively (over-approximating survival, so
        // discharge is never over-inferred); precise let-binder provenance is deferred to Step 5.
        for {
          argUsage  <- collect(args.head, env, carrierEffects)
          bodyUsage <- collect(body, env - param.value, carrierEffects)
          stored     = paramType.exists(pt => EffectCarriers.carrierHeaded(pt.value.signature, carrierEffects.keySet))
        } yield Usage(
          bodyUsage.effectful || (argUsage.effectful && !stored),
          argUsage.usedEffects ++ bodyUsage.usedEffects,
          argUsage.survivingParamEffects ++ bodyUsage.survivingParamEffects
        )
      case _                                                           =>
        // Non-value-reference head (applied parameter, nested application): no signature to read; conservative.
        for {
          headUsage <- collect(expr.as(head), env, carrierEffects)
          argUsage  <- args.foldMapM(collect(_, env, carrierEffects))
        } yield Usage(
          effectful = false,
          headUsage.usedEffects ++ argUsage.usedEffects,
          headUsage.survivingParamEffects ++ argUsage.survivingParamEffects
        )
    }
  }
}

object EffectUsageCollector {

  /** One subtree's accounting: whether its static result is effectful (carrier-headed), the user-facing effect
    * abilities performed anywhere within it, and the parameter-originated effects still reaching its result
    * undischarged (the discharge-inference provenance).
    */
  case class Usage(effectful: Boolean, usedEffects: Set[AbilityFQN], survivingParamEffects: Set[AbilityFQN])

  object Usage {
    val pure: Usage = Usage(effectful = false, Set.empty, Set.empty)

    given cats.Monoid[Usage] with {
      def empty: Usage                       = Usage.pure
      def combine(a: Usage, b: Usage): Usage =
        Usage(a.effectful || b.effectful, a.usedEffects ++ b.usedEffects, a.survivingParamEffects ++ b.survivingParamEffects)
    }
  }
}
