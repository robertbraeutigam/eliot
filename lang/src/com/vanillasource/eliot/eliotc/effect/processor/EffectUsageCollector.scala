package com.vanillasource.eliot.eliotc.effect.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.core.fact.TypeStack
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
  *     effect); read by the declared-effects subset check ([[DeclaredEffectChecker]]) and the `Inf` propagation it
  *     implies.
  *   - `effectful` — whether the body's static result is effectful (carrier-headed): a callee spine via
  *     `resultEffectful`, a parameter reference via its declared type being headed by one of the value's own carrier
  *     binders; read by the "declared pure but performs effects" fail-safe.
  *
  * Everything here is *signature-derivable* and definition-local — sequencing never changes *which* effects a body
  * performs, so this accounting is independent of the checker's bind decisions. No bind-position, branch-position or
  * machinery-sniffing logic survives from the desugarer; the one carrier-annotation read kept is the
  * immediately-applied-lambda (`let`) storage rule, mirroring the checker's own: an effectful bound value makes the
  * `let` effectful unless the binder is annotated carrier-typed (deliberate storage).
  */
class EffectUsageCollector(calleeSignatures: CalleeSignatures) {
  import EffectUsageCollector.*

  /** Collect the usage of `expr`. `env` maps in-scope value parameters to their declared types; `carrier` is the
    * current value's higher-kinded, ability-constrained carrier binder names.
    */
  def collect(
      expr: Sourced[OperatorResolvedExpression],
      env: Map[String, OperatorResolvedExpression],
      carrier: Set[String]
  )(using Platform): CompilerIO[Usage] =
    expr.value match {
      case _: IntegerLiteral | _: StringLiteral         => Usage.pure.pure[CompilerIO]
      case ParameterReference(name)                     =>
        Usage(env.get(name.value).exists(EffectCarriers.carrierHeaded(_, carrier)), Set.empty).pure[CompilerIO]
      case FunctionLiteral(name, _, lambdaBody)         =>
        // The lambda *value* is pure (a function); its body's performed effects still propagate.
        collect(lambdaBody, env - name.value, carrier).map(_.copy(effectful = false))
      case _: ValueReference | _: FunctionApplication   =>
        collectApplication(expr, env, carrier)
    }

  private def collectApplication(
      expr: Sourced[OperatorResolvedExpression],
      env: Map[String, OperatorResolvedExpression],
      carrier: Set[String]
  )(using platform: Platform): CompilerIO[Usage] = {
    val (head, args) = OperatorResolvedExpression.spine(expr.value)
    head match {
      case ValueReference(fqn, _)                                      =>
        for {
          info     <- calleeSignatures.infoFor(expr.as(fqn.value), platform)
          argUsage <- args.foldMapM(collect(_, env, carrier))
          effectful = info.resultEffectful(args.size)
        } yield Usage(effectful, (if (effectful) info.effectAbilities else Set.empty) ++ argUsage.usedEffects)
      case FunctionLiteral(param, paramType, body) if args.sizeIs == 1 =>
        // An immediately-applied lambda `(x -> body)(arg)` — a `let`. An effectful bound value makes the let
        // effectful (the checker sequences it) unless the binder is annotated carrier-typed (deliberate storage).
        for {
          argUsage  <- collect(args.head, env, carrier)
          bodyUsage <- collect(body, env - param.value, carrier)
          stored     = paramType.exists(pt => EffectCarriers.carrierHeaded(pt.value.signature, carrier))
        } yield Usage(
          bodyUsage.effectful || (argUsage.effectful && !stored),
          argUsage.usedEffects ++ bodyUsage.usedEffects
        )
      case _                                                           =>
        // Non-value-reference head (applied parameter, nested application): no signature to read; conservative.
        for {
          headUsage <- collect(expr.as(head), env, carrier)
          argUsage  <- args.foldMapM(collect(_, env, carrier))
        } yield Usage(effectful = false, headUsage.usedEffects ++ argUsage.usedEffects)
    }
  }
}

object EffectUsageCollector {

  /** One subtree's accounting: whether its static result is effectful (carrier-headed), and the user-facing effect
    * abilities performed anywhere within it.
    */
  case class Usage(effectful: Boolean, usedEffects: Set[AbilityFQN])

  object Usage {
    val pure: Usage = Usage(effectful = false, Set.empty)

    given cats.Monoid[Usage] with {
      def empty: Usage                       = Usage.pure
      def combine(a: Usage, b: Usage): Usage = Usage(a.effectful || b.effectful, a.usedEffects ++ b.usedEffects)
    }
  }
}
