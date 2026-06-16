package com.vanillasource.eliot.eliotc.monomorphize.eval

import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.monomorphize.check.SemExpression
import com.vanillasource.eliot.eliotc.monomorphize.domain.*
import com.vanillasource.eliot.eliotc.monomorphize.domain.SemValue.*
import com.vanillasource.eliot.eliotc.monomorphize.fact.GroundValue

/** Pure, synchronous NbE evaluator over the checker's [[SemExpression]] output. Mirrors [[Evaluator.eval]] but reads the
  * already-evaluated [[SemValue]] type arguments straight out of [[SemExpression.ValueReference]] instead of
  * re-evaluating ORE.
  *
  * Used by [[com.vanillasource.eliot.eliotc.monomorphize.check.PostDrainQuoter]] to materialise compile-time-constant
  * sub-terms — the ones whose value is fully determined by erased `[]`-bound parameters — into runtime literals and
  * constructor applications. It is never run on ordinary runtime code: the quoter only evaluates a sub-term once it has
  * established that the sub-term references an erased parameter and nothing runtime.
  *
  * @param lookupTopDef
  *   Resolve a top-level definition (the checker's binding cache). A missing binding becomes a stuck [[VNeutral]], which
  *   prevents materialisation (read-back fails) rather than silently mis-evaluating.
  */
class SemExpressionEvaluator(lookupTopDef: ValueFQN => Option[SemValue]) {

  /** Evaluate a [[SemExpression]] to a semantic value under the given environment. */
  def eval(env: Env, expr: SemExpression): SemValue = expr.expression match {
    case SemExpression.IntegerLiteral(value) =>
      VConst(GroundValue.Direct(value.value, Evaluator.bigIntGroundType))

    case SemExpression.StringLiteral(value) =>
      VConst(GroundValue.Direct(value.value, Evaluator.stringGroundType))

    case SemExpression.ParameterReference(name) =>
      env
        .lookupByName(name.value)
        .getOrElse(VNeutral(NeutralHead.VVar(env.level, name.value), Spine.SNil))

    case SemExpression.ValueReference(vfqn, typeArgs) =>
      val base = lookupTopDef(vfqn.value).getOrElse(
        VNeutral(NeutralHead.VVar(env.level, vfqn.value.name.name), Spine.SNil)
      )
      // The type arguments are already SemValues (the checker evaluated them); thread them into the value's spine.
      typeArgs.foldLeft(base)(Evaluator.applyValue)

    case SemExpression.FunctionApplication(target, argument) =>
      Evaluator.applyValue(eval(env, target.value), eval(env, argument.value))

    case SemExpression.FunctionLiteral(paramName, _, body) =>
      VLam(paramName.value, arg => eval(env.bind(paramName.value, arg), body.value))
  }
}
