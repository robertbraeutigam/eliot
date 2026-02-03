package com.vanillasource.eliot.eliotc.used2

import cats.Monad
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.eval.fact.Value
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.monomorphize.fact.{MonomorphicExpression, MonomorphicValue}
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.SingleKeyTypeProcessor
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.used2.UsedNamesState.*

import scala.annotation.tailrec

/** Processor that collects all reachable named values from a given root named value.
  *
  * This processor works with monomorphized facts and tracks:
  *   - Type parameters each value is used with
  *   - Statistics about how many arguments are applied directly to each value
  */
class UsedNamesProcessor extends SingleKeyTypeProcessor[UsedNames.Key] with Logging {

  override protected def generateFact(key: UsedNames.Key): CompilerIO[Unit] =
    for {
      state <- processValue(key.rootFQN, Seq.empty).runS(UsedNamesState())
      _     <- registerFactIfClear(getUsedNames(key.rootFQN, state))
    } yield ()

  private def processValue(vfqn: ValueFQN, typeArgs: Seq[Value]): UsedNamesIO[Unit] =
    isVisited(vfqn, typeArgs).ifM(
      Monad[CompilerIO].unit.liftToUsedNames,
      for {
        _                <- markVisited(vfqn, typeArgs)
        monomorphicMaybe <- getFact(MonomorphicValue.Key(vfqn, typeArgs)).liftToUsedNames
        _                <- monomorphicMaybe.traverse_(processMonomorphicValue)
      } yield ()
    )

  private def processMonomorphicValue(mv: MonomorphicValue): UsedNamesIO[Unit] =
    mv.runtime.traverse_(sourcedExpr => processExpression(sourcedExpr.value))

  @tailrec
  private def processExpression(expr: MonomorphicExpression.Expression): UsedNamesIO[Unit] =
    expr match {
      case MonomorphicExpression.FunctionApplication(target, argument) =>
        processApplicationChain(target.value.expression, 1, Seq(argument))

      case MonomorphicExpression.MonomorphicValueReference(vfqn, typeArgs) =>
        processValueReference(vfqn.value, typeArgs, 0)

      case MonomorphicExpression.FunctionLiteral(_, _, body) =>
        processExpression(body.value.expression)

      case MonomorphicExpression.IntegerLiteral(_) =>
        Monad[CompilerIO].unit.liftToUsedNames

      case MonomorphicExpression.StringLiteral(_) =>
        Monad[CompilerIO].unit.liftToUsedNames

      case MonomorphicExpression.ParameterReference(_) =>
        Monad[CompilerIO].unit.liftToUsedNames
    }

  /** Process a chain of function applications to count how many arguments are applied directly to a value reference.
    *
    * @param expr
    *   The current expression in the chain (target of an application)
    * @param applicationCount
    *   Number of applications counted so far
    * @param nestedArguments
    *   Arguments collected from nested applications (need to be processed after finding the base)
    */
  @tailrec
  private def processApplicationChain(
      expr: MonomorphicExpression.Expression,
      applicationCount: Int,
      nestedArguments: Seq[Sourced[MonomorphicExpression]]
  ): UsedNamesIO[Unit] = {
    def processNestedArguments: UsedNamesIO[Unit] =
      nestedArguments.traverse_(arg => processExpression(arg.value.expression))

    expr match {
      case MonomorphicExpression.FunctionApplication(target, argument) =>
        processApplicationChain(target.value.expression, applicationCount + 1, nestedArguments :+ argument)

      case MonomorphicExpression.MonomorphicValueReference(vfqn, typeArgs) =>
        processValueReference(vfqn.value, typeArgs, applicationCount) >> processNestedArguments

      case MonomorphicExpression.FunctionLiteral(_, _, body) =>
        processExpression(body.value.expression) >> processNestedArguments

      case MonomorphicExpression.ParameterReference(_) =>
        processNestedArguments

      case _ =>
        processNestedArguments
    }
  }

  private def processValueReference(
      vfqn: ValueFQN,
      typeArgs: Seq[Value],
      applicationCount: Int
  ): UsedNamesIO[Unit] =
    for {
      _ <- recordUsage(vfqn, typeArgs, applicationCount)
      _ <- processValue(vfqn, typeArgs)
    } yield ()
}
