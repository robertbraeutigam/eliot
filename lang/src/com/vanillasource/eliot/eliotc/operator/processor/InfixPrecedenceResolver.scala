package com.vanillasource.eliot.eliotc.operator.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.ast.fact.Fixity.Associativity
import com.vanillasource.eliot.eliotc.ast.fact.PrecedenceDeclaration.Relation
import com.vanillasource.eliot.eliotc.core.fact.TypeStack
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import TokenClassifier.{InfixOp, Operand, Token, outlinedStack}
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedExpression
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.resolve.fact.ResolvedValue
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerAbort

import scala.annotation.tailrec

object InfixPrecedenceResolver {

  def resolve(tokens: Seq[Token]): CompilerIO[OperatorResolvedExpression] = {
    val operands  = tokens.collect { case Operand(p) => p }
    val operators = tokens.collect { case i: InfixOp => i }

    if (operators.isEmpty) {
      operands.head.value.signature.pure[CompilerIO]
    } else {
      for {
        precOrder <- buildPrecedenceOrder(operators)
        result    <- resolveInfixRecursive(operands, operators, precOrder)
      } yield result
    }
  }

  private case class PrecedenceOrder(
      higherThan: Map[ValueFQN, Set[ValueFQN]],
      sameAs: Map[ValueFQN, Set[ValueFQN]]
  ) {
    def compare(a: ValueFQN, b: ValueFQN): Option[Int] =
      if (a === b) Some(0)
      else if (sameAs.getOrElse(a, Set.empty).contains(b)) Some(0)
      else if (isHigher(a, b)) Some(1)
      else if (isHigher(b, a)) Some(-1)
      else None

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

  private def buildPrecedenceOrder(operators: Seq[InfixOp]): CompilerIO[PrecedenceOrder] = {
    val vfqns = operators.map(_.vfqn).distinct
    for {
      rvs <- vfqns.traverse(v => getFactOrAbort(ResolvedValue.Key(v)).map(v -> _))
    } yield {
      val higherPairs = rvs.flatMap { (vfqn, rv) =>
        rv.precedence.flatMap { decl =>
          val targets = decl.targets.map(_.value)
          decl.relation match {
            case Relation.Above => targets.map(vfqn -> _)
            case Relation.Below => targets.map(_ -> vfqn)
            case Relation.At    => Seq.empty
          }
        }
      }
      val samePairs = rvs.flatMap { (vfqn, rv) =>
        rv.precedence.flatMap { decl =>
          decl.relation match {
            case Relation.At => decl.targets.flatMap(t => Seq(vfqn -> t.value, t.value -> vfqn))
            case _           => Seq.empty
          }
        }
      }
      PrecedenceOrder(
        higherPairs.groupMap(_._1)(_._2).map((k, v) => k -> v.toSet),
        samePairs.groupMap(_._1)(_._2).map((k, v) => k -> v.toSet)
      )
    }
  }

  private def resolveInfixRecursive(
      operands: Seq[Sourced[TypeStack[OperatorResolvedExpression]]],
      operators: Seq[InfixOp],
      order: PrecedenceOrder
  ): CompilerIO[OperatorResolvedExpression] =
    if (operators.isEmpty) {
      operands.head.value.signature.pure[CompilerIO]
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
        val leftStack  = outlinedStack(leftOperands, left)
        val rightStack = outlinedStack(rightOperands, right)
        makeCurriedInfix(splitOp.part, leftStack, rightStack)
      }
    }

  private def findSplitPoint(operators: Seq[InfixOp], order: PrecedenceOrder): CompilerIO[Int] =
    for {
      weakestIdx      <- findWeakestOperator(operators, order)
      sameLevelIndices = operators.indices.filter(i => order.compare(operators(i).vfqn, operators(weakestIdx).vfqn).contains(0))
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
      case Seq(Associativity.Left)  => sameLevelIndices.last.pure[CompilerIO]
      case Seq(Associativity.Right) => sameLevelIndices.head.pure[CompilerIO]
      case Seq(Associativity.None) if sameLevelIndices.length == 1 => sameLevelIndices.head.pure[CompilerIO]
      case Seq(Associativity.None)  =>
        compilerAbort(
          operators(sameLevelIndices.head).part.as(
            s"Non-associative operator '${operators(sameLevelIndices.head).vfqn.name.name}' cannot be chained."
          )
        )
      case _                        =>
        compilerAbort(operators(sameLevelIndices.head).part.as("Operators at same precedence level have different associativity."))
    }

  private def makeCurriedInfix(
      op: Sourced[TypeStack[OperatorResolvedExpression]],
      left: Sourced[TypeStack[OperatorResolvedExpression]],
      right: Sourced[TypeStack[OperatorResolvedExpression]]
  ): OperatorResolvedExpression = {
    val app1     = OperatorResolvedExpression.FunctionApplication(op, left)
    val combined = outlinedStack(Seq(op, left), app1)
    OperatorResolvedExpression.FunctionApplication(combined, right)
  }
}
