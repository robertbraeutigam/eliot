package com.vanillasource.eliot.eliotc.operator.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.ast.fact.Fixity.Associativity
import com.vanillasource.eliot.eliotc.ast.fact.PrecedenceDeclaration.Relation
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import TokenClassifier.{InfixOp, Operand, Token, outlinedExpr}
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedExpression
import com.vanillasource.eliot.eliotc.platform.Platform
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.resolve.fact.ResolvedValue
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerAbort

import scala.annotation.tailrec

object InfixPrecedenceResolver {

  def resolve(tokens: Seq[Token])(using Platform): CompilerIO[OperatorResolvedExpression] = {
    val operands  = tokens.collect { case Operand(p) => p }
    val operators = tokens.collect { case i: InfixOp => i }

    if (operators.isEmpty) {
      operands.head.value.pure[CompilerIO]
    } else {
      for {
        precOrder <- buildPrecedenceOrder(operators)
        result    <- resolveInfixRecursive(operands, operators, precOrder)
      } yield result
    }
  }

  private case class PrecedenceOrder(
      higherThan: Map[ValueFQN, Set[ValueFQN]],
      sameAs: Map[ValueFQN, Set[ValueFQN]],
      applyComponent: Set[ValueFQN]
  ) {
    def compare(a: ValueFQN, b: ValueFQN): Option[Int] =
      if (a === b) Some(0)
      else if (sameAs.getOrElse(a, Set.empty).contains(b)) Some(0)
      else if (isHigher(a, b)) Some(1)
      else if (isHigher(b, a)) Some(-1)
      else anchoredComparison(a, b)

    /** Fallback for two operators with no explicitly declared ordering. `apply` (application) is the guaranteed
      * unique maximum — the resolver forbids `above`/`at apply` — so an operator that shares `apply`'s precedence
      * island (is connected to it through any chain of `above`/`below`/`at`, in any direction) belongs to the
      * tightest-binding tier and binds tighter than any operator sitting in an island unconnected to `apply`. Two
      * operators on the same side (both connected, or both unconnected) keep no relative ordering, so a genuinely
      * ambiguous pair still errors. */
    private def anchoredComparison(a: ValueFQN, b: ValueFQN): Option[Int] =
      (applyComponent.contains(a), applyComponent.contains(b)) match {
        case (true, false) => Some(1)
        case (false, true) => Some(-1)
        case _             => None
      }

    private def isHigher(a: ValueFQN, b: ValueFQN): Boolean = {
      val bTargets = sameAs.getOrElse(b, Set.empty) + b

      @tailrec
      def bfs(queue: List[ValueFQN], visited: Set[ValueFQN]): Boolean =
        queue match {
          case Nil             => false
          case current :: rest =>
            if (visited.contains(current)) bfs(rest, visited)
            else {
              val lower = higherThan.getOrElse(current, Set.empty)
              if (lower.exists(bTargets.contains)) true
              else {
                val newNodes = (lower ++ lower.flatMap(l => sameAs.getOrElse(l, Set.empty))) -- visited - current
                bfs(rest ++ newNodes, visited + current)
              }
            }
        }

      bfs((sameAs.getOrElse(a, Set.empty) + a).toList, Set.empty)
    }
  }

  /** Build the precedence order over the *transitive closure* of the present operators' precedence declarations
    * (following each declaration's targets). The closure is required so that connectivity to `apply` is computed
    * correctly even when the connecting operator does not itself appear in the expression — e.g. `+` declared
    * `above p` where `p` is `below apply` puts `+` in `apply`'s island through the absent `p`. */
  private def buildPrecedenceOrder(operators: Seq[InfixOp])(using platform: Platform): CompilerIO[PrecedenceOrder] =
    fetchPrecedenceClosure(operators.map(_.vfqn).toSet, Map.empty).map { rvs =>
      val decls       = rvs.toSeq.flatMap((vfqn, rv) => rv.precedence.map(vfqn -> _))
      val higherPairs = decls.flatMap { (vfqn, decl) =>
        val targets = decl.targets.map(_.value)
        decl.relation match {
          case Relation.Above => targets.map(vfqn -> _)
          case Relation.Below => targets.map(_ -> vfqn)
          case Relation.At    => Seq.empty
        }
      }
      val samePairs   = decls.flatMap { (vfqn, decl) =>
        decl.relation match {
          case Relation.At => decl.targets.flatMap(t => Seq(vfqn -> t.value, t.value -> vfqn))
          case _           => Seq.empty
        }
      }
      PrecedenceOrder(
        higherPairs.groupMap(_._1)(_._2).map((k, v) => k -> v.toSet),
        samePairs.groupMap(_._1)(_._2).map((k, v) => k -> v.toSet),
        applyComponentOf(higherPairs ++ samePairs)
      )
    }

  /** Fetch `ResolvedValue`s transitively along precedence-declaration targets, starting from the present
    * operators. `apply` itself is never fetched — it declares no precedence and appears only as a target node. */
  private def fetchPrecedenceClosure(
      frontier: Set[ValueFQN],
      acc: Map[ValueFQN, ResolvedValue]
  )(using platform: Platform): CompilerIO[Map[ValueFQN, ResolvedValue]] = {
    val toFetch = (frontier -- acc.keySet - ValueFQN.applyFQN).toList
    if (toFetch.isEmpty) acc.pure[CompilerIO]
    else
      for {
        fetched <- toFetch.traverse(v => getFactOrAbort(ResolvedValue.Key(v, platform)).map(v -> _))
        targets  = fetched.flatMap((_, rv) => rv.precedence.flatMap(_.targets.map(_.value))).toSet
        result  <- fetchPrecedenceClosure(targets, acc ++ fetched)
      } yield result
  }

  /** The set of names in `apply`'s connected component over the *undirected* precedence graph (an edge for every
    * `above`/`below`/`at` relation). Membership marks the tightest-binding tier — see [[anchoredComparison]]. */
  private def applyComponentOf(edges: Seq[(ValueFQN, ValueFQN)]): Set[ValueFQN] = {
    val adjacency = (edges ++ edges.map(_.swap)).groupMap(_._1)(_._2).map((k, v) => k -> v.toSet)

    @tailrec
    def bfs(queue: List[ValueFQN], visited: Set[ValueFQN]): Set[ValueFQN] =
      queue match {
        case Nil                                  => visited
        case current :: rest if visited(current) => bfs(rest, visited)
        case current :: rest                      =>
          bfs(rest ++ adjacency.getOrElse(current, Set.empty).toList, visited + current)
      }

    bfs(List(ValueFQN.applyFQN), Set.empty)
  }

  private def resolveInfixRecursive(
      operands: Seq[Sourced[OperatorResolvedExpression]],
      operators: Seq[InfixOp],
      order: PrecedenceOrder
  ): CompilerIO[OperatorResolvedExpression] =
    if (operators.isEmpty) {
      operands.head.value.pure[CompilerIO]
    } else if (operators.length == 1) {
      makeCurriedInfix(operators.head.part, operands.head, operands(1)).pure[CompilerIO]
    } else {
      for {
        splitIdx     <- findSplitPoint(operators, order)
        splitOp       = operators(splitIdx)
        leftOperands  = operands.take(splitIdx + 1)
        rightOperands = operands.drop(splitIdx + 1)
        leftOps       = operators.take(splitIdx)
        rightOps      = operators.drop(splitIdx + 1)
        left         <- resolveInfixRecursive(leftOperands, leftOps, order)
        right        <- resolveInfixRecursive(rightOperands, rightOps, order)
      } yield {
        val leftExpr  = outlinedExpr(leftOperands, left)
        val rightExpr = outlinedExpr(rightOperands, right)
        makeCurriedInfix(splitOp.part, leftExpr, rightExpr)
      }
    }

  private def findSplitPoint(operators: Seq[InfixOp], order: PrecedenceOrder): CompilerIO[Int] =
    for {
      weakestIdx      <- findWeakestOperator(operators, order)
      sameLevelIndices =
        operators.indices.filter(i => order.compare(operators(i).vfqn, operators(weakestIdx).vfqn).contains(0))
      splitIdx        <- pickSplitByAssociativity(operators, sameLevelIndices)
    } yield splitIdx

  private def findWeakestOperator(operators: Seq[InfixOp], order: PrecedenceOrder): CompilerIO[Int] =
    (1 until operators.length).toList.foldM(0) { (weakestIdx, i) =>
      order.compare(operators(i).vfqn, operators(weakestIdx).vfqn) match {
        case Some(c) if c < 0 => i.pure[CompilerIO]
        case Some(_)          => weakestIdx.pure[CompilerIO]
        case None             =>
          compilerAbort(
            operators(i).part.as(
              s"Operators '${operators(i).vfqn.name.name}' and '${operators(weakestIdx).vfqn.name.name}' have no defined relative precedence."
            )
          )
      }
    }

  private def pickSplitByAssociativity(operators: Seq[InfixOp], sameLevelIndices: Seq[Int]): CompilerIO[Int] =
    sameLevelIndices.map(i => operators(i).associativity).distinct match {
      case Seq(Associativity.Left)                                 => sameLevelIndices.last.pure[CompilerIO]
      case Seq(Associativity.Right)                                => sameLevelIndices.head.pure[CompilerIO]
      case Seq(Associativity.None) if sameLevelIndices.length == 1 => sameLevelIndices.head.pure[CompilerIO]
      case Seq(Associativity.None)                                 =>
        compilerAbort(
          operators(sameLevelIndices.head).part.as(
            s"Non-associative operator '${operators(sameLevelIndices.head).vfqn.name.name}' cannot be chained."
          )
        )
      case _                                                       =>
        compilerAbort(
          operators(sameLevelIndices.head).part.as("Operators at same precedence level have different associativity.")
        )
    }

  private def makeCurriedInfix(
      op: Sourced[OperatorResolvedExpression],
      left: Sourced[OperatorResolvedExpression],
      right: Sourced[OperatorResolvedExpression]
  ): OperatorResolvedExpression = {
    val app1     = OperatorResolvedExpression.FunctionApplication(op, left)
    val combined = outlinedExpr(Seq(op, left), app1)
    OperatorResolvedExpression.FunctionApplication(combined, right)
  }
}
