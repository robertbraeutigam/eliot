package com.vanillasource.eliot.eliotc.monomorphize.eval

import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.monomorphize.check.SemExpression
import com.vanillasource.eliot.eliotc.monomorphize.domain.*

/** Pure, synchronous NbE evaluator over the checker's [[SemExpression]] output. Shares the [[NbeEvaluator]] traversal
  * with the ORE [[Evaluator]]; it only differs in [[decompose]], reading the already-evaluated [[SemValue]] type
  * arguments straight out of [[SemExpression.ValueReference]] instead of re-evaluating ORE.
  *
  * Used by [[com.vanillasource.eliot.eliotc.monomorphize.check.PostDrainQuoter]] to materialise compile-time-constant
  * sub-terms — the ones whose value is fully determined by erased `[]`-bound parameters — into runtime literals and
  * constructor applications. It is never run on ordinary runtime code: the quoter only evaluates a sub-term once it has
  * established that the sub-term references an erased parameter and nothing runtime.
  *
  * @param lookupTopDef
  *   Resolve a top-level definition (the checker's binding cache). A missing binding becomes a stuck [[VNeutral]],
  *   which prevents materialisation (read-back fails) rather than silently mis-evaluating.
  */
class SemExpressionEvaluator(lookupTopDef: ValueFQN => Option[SemValue])
    extends NbeEvaluator[SemExpression](lookupTopDef) {

  override protected def decompose(env: Env, expr: SemExpression): NbeEvaluator.Term[SemExpression] =
    expr.expression match {
      case SemExpression.IntegerLiteral(value) =>
        NbeEvaluator.Term.IntegerLiteral(value.value)

      case SemExpression.StringLiteral(value) =>
        NbeEvaluator.Term.StringLiteral(value.value)

      case SemExpression.ParameterReference(name) =>
        NbeEvaluator.Term.ParameterReference(name.value)

      case SemExpression.ValueReference(vfqn, typeArgs) =>
        // The type arguments are already SemValues (the checker evaluated them); pass them through unchanged.
        NbeEvaluator.Term.ValueReference(vfqn.value, typeArgs)

      case SemExpression.FunctionApplication(target, argument) =>
        NbeEvaluator.Term.FunctionApplication(target.value, argument.value)

      case SemExpression.FunctionLiteral(paramName, _, body) =>
        NbeEvaluator.Term.FunctionLiteral(paramName.value, body.value)
    }
}
