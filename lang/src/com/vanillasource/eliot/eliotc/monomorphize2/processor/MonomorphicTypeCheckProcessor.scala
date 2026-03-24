package com.vanillasource.eliot.eliotc.monomorphize2.processor

import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.core.fact.TypeStack
import com.vanillasource.eliot.eliotc.eval.fact.Value
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.monomorphize2.fact.*
import com.vanillasource.eliot.eliotc.monomorphize2.processor.TypeCheckState.TypeGraphIO
import com.vanillasource.eliot.eliotc.operator.fact.{OperatorResolvedExpression, OperatorResolvedValue}
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.TransformationProcessor
import com.vanillasource.eliot.eliotc.source.content.Sourced

class MonomorphicTypeCheckProcessor
    extends TransformationProcessor[OperatorResolvedValue.Key, MonomorphicValue.Key](key =>
      OperatorResolvedValue.Key(key.vfqn)
    )
    with Logging {

  override protected def generateFromKeyAndFact(
      key: MonomorphicValue.Key,
      resolvedValue: OperatorResolvedValue
  ): CompilerIO[MonomorphicValue] =
    for {
      endState             <- typeCheck(resolvedValue.typeStack, resolvedValue.runtime).runS(TypeCheckState())
      _                    <- debug[CompilerIO](s"Constraints (of ${key.vfqn.show}): ${endState.constraints.show}")
      solution             <- solve(endState.constraints)
      _                    <- debug[CompilerIO](s"Solution (of ${key.vfqn.show}): ${solution.show}")
      (signature, runtime) <- typeSubstitute(solution, resolvedValue.typeStack, resolvedValue.runtime)
    } yield MonomorphicValue(
      key.vfqn,
      key.typeArguments,
      signature,
      runtime
    )

  private def typeCheck(
      typeExpressions: Sourced[TypeStack[OperatorResolvedExpression]],
      runtime: Option[Sourced[OperatorResolvedExpression]]
  ): TypeGraphIO[Unit] =
    ???

  private def solve(constraints: Constraints): CompilerIO[Solution] =
    ???

  private def typeSubstitute(
      solution: Solution,
      typeExpressions: Sourced[TypeStack[OperatorResolvedExpression]],
      runtime: Option[Sourced[OperatorResolvedExpression]]
  ): CompilerIO[(Value, Option[Sourced[MonomorphicExpression.Expression]])] =
    ???

}
