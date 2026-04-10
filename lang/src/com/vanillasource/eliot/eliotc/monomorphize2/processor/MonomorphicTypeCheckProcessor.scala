package com.vanillasource.eliot.eliotc.monomorphize2.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.eval.fact.Types.{bigIntType, stringType}
import com.vanillasource.eliot.eliotc.eval.fact.{ExpressionValue, Value}
import com.vanillasource.eliot.eliotc.eval.util.Evaluator
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.monomorphize2.fact.*
import com.vanillasource.eliot.eliotc.monomorphize2.typecheck.constraints.ConstraintExtract.collectConstraints
import com.vanillasource.eliot.eliotc.monomorphize2.typecheck.constraints.{Constraints, TypeCheckState}
import com.vanillasource.eliot.eliotc.monomorphize2.typecheck.solution.ConstraintSolver.solve
import com.vanillasource.eliot.eliotc.monomorphize2.typecheck.solution.Solution
import com.vanillasource.eliot.eliotc.operator.fact.{OperatorResolvedExpression, OperatorResolvedValue}
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.TransformationProcessor
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerAbort

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
      _                    <-
        debug[CompilerIO](
          s"Type checking ${key.vfqn.show}, with type arguments: ${key.specifiedTypeArguments.map(_.show).mkString(", ")}"
        )
      endState             <- collectConstraints(key, resolvedValue).runS(TypeCheckState())
      _                    <- Constraints.debugConstraints(endState.constraints)
      solution             <- solve(endState.constraints, endState.shortIds)
      _                    <- debug[CompilerIO](s"Solution (of ${key.vfqn.show}): ${solution.show}")
      (signature, runtime) <- typeSubstitute(key, solution, endState, resolvedValue)
    } yield MonomorphicValue(
      key.vfqn,
      key.specifiedTypeArguments,
      Seq.empty, // TODO: left out for now
      signature,
      runtime
    )

  // --- Phase 3: Type substitution and MonomorphicExpression building ---
  //
  // The constraint extractor recorded, for each ORE node it walked, the assumed (expected-from-
  // parent) ORE type at that node into `endState.nodeAssumedTypes`. This walk reads from that
  // side-table by node identity to discover each node's expected type, then evaluates and
  // resolves it through the Solution to obtain a concrete `Value`. There is no replay of fresh
  // unification variables and no top-down `assumedType` parameter — every node is independent.

  private def typeSubstitute(
      key: MonomorphicValue.Key,
      solution: Solution,
      endState: TypeCheckState,
      resolvedValue: OperatorResolvedValue
  ): CompilerIO[(Value, Option[Sourced[MonomorphicExpression.Expression]])] =
    for {
      typeArgEVs    <- key.specifiedTypeArguments.toList.traverse(arg => Evaluator.evaluate(arg))
      typeExprValue <- Evaluator.evaluate(resolvedValue.typeStack.map(_.signature))
      signature     <- Evaluator.applyExpressionTypeArgs(typeExprValue, typeArgEVs, resolvedValue.name)
      paramTypes    <- resolveParameterTypes(endState, solution)
      runtime       <- resolvedValue.runtime.traverse { body =>
                         buildExpression(body, paramTypes, endState, solution).map(body.as)
                       }
    } yield (signature, runtime.map(_.map(_.expression)))

  private def resolveParameterTypes(
      endState: TypeCheckState,
      solution: Solution
  ): CompilerIO[Map[String, Value]] =
    endState.parameterTypes.toList
      .traverse { case (name, sourced) =>
        Evaluator.evaluate(sourced).map { ev =>
          ExpressionValue.concreteValueOf(solution.resolveExpressionValue(ev)).map(name -> _)
        }
      }
      .map(_.flatten.toMap)

  private def buildExpression(
      source: Sourced[OperatorResolvedExpression],
      paramTypes: Map[String, Value],
      endState: TypeCheckState,
      solution: Solution
  ): CompilerIO[MonomorphicExpression] =
    source.value match {
      case OperatorResolvedExpression.IntegerLiteral(value) =>
        MonomorphicExpression(bigIntType, MonomorphicExpression.IntegerLiteral(value)).pure[CompilerIO]

      case OperatorResolvedExpression.StringLiteral(value) =>
        MonomorphicExpression(stringType, MonomorphicExpression.StringLiteral(value)).pure[CompilerIO]

      case OperatorResolvedExpression.ParameterReference(name) =>
        for {
          fallback <- nodeType(source, endState, solution)
        } yield {
          val paramType = paramTypes.getOrElse(name.value, fallback)
          MonomorphicExpression(paramType, MonomorphicExpression.ParameterReference(name))
        }

      case OperatorResolvedExpression.ValueReference(vfqn, _) =>
        for {
          ty <- nodeType(source, endState, solution)
        } yield MonomorphicExpression(
          ty,
          MonomorphicExpression.MonomorphicValueReference(vfqn, Seq.empty)
        )

      case OperatorResolvedExpression.FunctionApplication(target, arg) =>
        for {
          ty         <- nodeType(source, endState, solution)
          targetExpr <- buildExpression(target, paramTypes, endState, solution)
          argExpr    <- buildExpression(arg, paramTypes, endState, solution)
        } yield MonomorphicExpression(
          ty,
          MonomorphicExpression.FunctionApplication(target.as(targetExpr), arg.as(argExpr))
        )

      case OperatorResolvedExpression.FunctionLiteral(paramName, _, body) =>
        for {
          ty       <- nodeType(source, endState, solution)
          bodyExpr <- buildExpression(body, paramTypes, endState, solution)
        } yield {
          val paramType = paramTypes.getOrElse(paramName.value, Value.Type)
          MonomorphicExpression(
            ty,
            MonomorphicExpression.FunctionLiteral(paramName, paramType, body.as(bodyExpr))
          )
        }
    }

  /** Look up the node's recorded assumed-type ORE in the side-table, evaluate it, resolve any
    * remaining unification variables through the Solution, reduce once more so that newly-
    * concrete arguments to native type constructors get applied, and project to a concrete
    * `Value`.
    */
  private def nodeType(
      source: Sourced[OperatorResolvedExpression],
      endState: TypeCheckState,
      solution: Solution
  ): CompilerIO[Value] =
    endState.nodeAssumedTypes.get(source) match {
      case Some(assumedOre) =>
        for {
          ev      <- Evaluator.evaluate(source.as(assumedOre))
          resolved = solution.resolveExpressionValue(ev)
          reduced <- Evaluator.reduce(resolved, source)
          result  <- ExpressionValue.concreteValueOf(reduced) match {
                       case Some(value) => value.pure[CompilerIO]
                       case None        =>
                         compilerAbort(source.as("Type at this position did not resolve to a concrete value."))
                     }
        } yield result
      case None             =>
        compilerAbort(source.as("Internal error: no assumed type recorded for this node."))
    }

}
