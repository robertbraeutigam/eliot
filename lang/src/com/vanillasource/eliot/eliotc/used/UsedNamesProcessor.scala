package com.vanillasource.eliot.eliotc.used

import cats.Monad
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.module.fact.{QualifiedName, ValueFQN}
import com.vanillasource.eliot.eliotc.monomorphize.fact.{GroundValue, MonomorphicExpression, MonomorphicValue}
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.SingleKeyTypeProcessor
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerAbort
import com.vanillasource.eliot.eliotc.used.UsedNamesState.*

import scala.annotation.tailrec

/** Processor that collects all reachable named values from a given root named value.
  *
  * This processor works with monomorphized facts and tracks:
  *   - Type parameters each value is used with
  *   - Statistics about how many arguments are applied directly to each value
  *
  * It is also the home of the **non-convergence backstop** (Deliverable A of `docs/monomorphization-keying-plan.md`):
  * because `used` is the codegen driver that walks the *breadth* of the monomorphic fact graph (`f[N] -> f[N-1] -> …`,
  * `loop[A] -> loop[Box[A]] -> …`), a divergent type-level recursion makes this DFS materialise specializations without
  * bound — today a hang/OOM. The recursion lives in this processor's own `processValue` descent (not in the global
  * `activeFactKeys` chain, which only reflects *depth* across fact generations and stays flat here, since every
  * `MonomorphicValue` is requested as a sibling of `used`), so the backstop tracks the chain of enclosing `processValue`
  * frames (`ancestors`) and, when more than [[maxNestedRepeats]] of them share the same `vfqn` with differing type
  * arguments, converts the runaway into a specific diagnostic instead of diverging. This is the fail-safe behind the
  * keying-plan's relevance/variance analysis (Deliverable B): legitimate recursion collapses or demotes and never trips;
  * the backstop only fires on genuinely divergent type-level recursion or analysis-missed positions.
  *
  * @param maxNestedRepeats
  *   How many nested `processValue` frames for one `vfqn` are tolerated before the specialization is declared
  *   non-converging. Generous on purpose (see [[UsedNamesProcessor.DefaultMaxNestedRepeats]]): a terminating recursion is
  *   bounded by the program's finite type structure and stays far below it, so only genuine divergence crosses it.
  */
class UsedNamesProcessor(maxNestedRepeats: Int = UsedNamesProcessor.DefaultMaxNestedRepeats)
    extends SingleKeyTypeProcessor[UsedNames.Key]
    with Logging {

  override protected def generateFact(key: UsedNames.Key): CompilerIO[Unit] =
    for {
      state <- (recordUsage(key.rootFQN, Seq.empty, 0) >> processValue(key.rootFQN, Seq.empty, Nil)).runS(
                 UsedNamesState()
               )
      _     <- registerFactIfClear(getUsedNames(key.rootFQN, state)).whenA(!state.failed)
    } yield ()

  /** @param ancestors
    *   The `vfqn`s of the enclosing `processValue` frames (the DFS stack of values currently being walked, innermost
    *   first), used by the non-convergence backstop to detect a runaway recursion.
    */
  private def processValue(vfqn: ValueFQN, typeArgs: Seq[GroundValue], ancestors: List[ValueFQN]): UsedNamesIO[Unit] =
    isVisited(vfqn, typeArgs).ifM(
      Monad[CompilerIO].unit.liftToUsedNames,
      for {
        _                <- markVisited(vfqn, typeArgs)
        monomorphicMaybe <- getFact(MonomorphicValue.Key(vfqn, typeArgs)).liftToUsedNames
        _                <- monomorphicMaybe.fold(markFailed()) { mv =>
                              checkConvergence(vfqn, mv.name, ancestors) >>
                                processMonomorphicValue(mv, vfqn :: ancestors)
                            }
      } yield ()
    )

  /** Trip the backstop when this `vfqn` already occurs more than [[maxNestedRepeats]] times among its enclosing frames —
    * a chain of nested specializations of the same value with differing type arguments, i.e. a non-converging recursion.
    */
  private def checkConvergence(
      vfqn: ValueFQN,
      name: Sourced[QualifiedName],
      ancestors: List[ValueFQN]
  ): UsedNamesIO[Unit] = {
    val nestedRepeats = ancestors.count(_ == vfqn)
    if (nestedRepeats > maxNestedRepeats) reportNonConvergence(name, nestedRepeats + 1).liftToUsedNames
    else Monad[CompilerIO].unit.liftToUsedNames
  }

  private def reportNonConvergence(name: Sourced[QualifiedName], depth: Int): CompilerIO[Unit] =
    compilerAbort(
      name.as(
        s"Monomorphization of '${name.value.name}' is not converging: specialized $depth levels deep with differing type arguments."
      ),
      Seq(
        "Type argument(s) change on every recursive step, so a new specialization is produced each time and the chain never closes.",
        "Make the varying argument(s) compile-time only (phantom), bound the recursion, or demote them to runtime."
      )
    )

  private def processMonomorphicValue(mv: MonomorphicValue, ancestors: List[ValueFQN]): UsedNamesIO[Unit] =
    mv.runtime.traverse_(sourcedExpr => processExpression(sourcedExpr.value, ancestors))

  @tailrec
  private def processExpression(expr: MonomorphicExpression.Expression, ancestors: List[ValueFQN]): UsedNamesIO[Unit] =
    expr match {
      case MonomorphicExpression.FunctionApplication(target, argument) =>
        processApplicationChain(target.value.expression, 1, Seq(argument), ancestors)

      case MonomorphicExpression.MonomorphicValueReference(vfqn, typeArgs) =>
        processValueReference(vfqn.value, typeArgs, 0, ancestors)

      case MonomorphicExpression.FunctionLiteral(_, _, body) =>
        processExpression(body.value.expression, ancestors)

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
    * @param ancestors
    *   The enclosing-frame chain threaded for the non-convergence backstop (see [[processValue]])
    */
  @tailrec
  private def processApplicationChain(
      expr: MonomorphicExpression.Expression,
      applicationCount: Int,
      nestedArguments: Seq[Sourced[MonomorphicExpression]],
      ancestors: List[ValueFQN]
  ): UsedNamesIO[Unit] = {
    def processNestedArguments: UsedNamesIO[Unit] =
      nestedArguments.traverse_(arg => processExpression(arg.value.expression, ancestors))

    expr match {
      case MonomorphicExpression.FunctionApplication(target, argument) =>
        processApplicationChain(target.value.expression, applicationCount + 1, nestedArguments :+ argument, ancestors)

      case MonomorphicExpression.MonomorphicValueReference(vfqn, typeArgs) =>
        processValueReference(vfqn.value, typeArgs, applicationCount, ancestors) >> processNestedArguments

      case MonomorphicExpression.FunctionLiteral(_, _, body) =>
        processExpression(body.value.expression, ancestors) >> processNestedArguments

      case MonomorphicExpression.ParameterReference(_) =>
        processNestedArguments

      case _ =>
        processNestedArguments
    }
  }

  private def processValueReference(
      vfqn: ValueFQN,
      typeArgs: Seq[GroundValue],
      applicationCount: Int,
      ancestors: List[ValueFQN]
  ): UsedNamesIO[Unit] =
    for {
      _ <- recordUsage(vfqn, typeArgs, applicationCount)
      _ <- processValue(vfqn, typeArgs, ancestors)
    } yield ()
}

object UsedNamesProcessor {

  /** Default nesting tolerance for the non-convergence backstop. 500 is deliberately generous: a terminating recursion
    * is bounded by the program's finite type structure and stays far below this, while a genuinely divergent recursion is
    * unbounded and always crosses it. Configurable per the keying plan (Deliverable A).
    */
  val DefaultMaxNestedRepeats: Int = 500
}
