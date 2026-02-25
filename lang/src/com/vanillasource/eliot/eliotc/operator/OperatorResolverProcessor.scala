package com.vanillasource.eliot.eliotc.operator

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.ast.fact.Fixity
import com.vanillasource.eliot.eliotc.ast.fact.Fixity.Associativity
import com.vanillasource.eliot.eliotc.ast.fact.PrecedenceDeclaration.Relation
import com.vanillasource.eliot.eliotc.core.fact.TypeStack
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.operator.OperatorResolvedValue
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.TransformationProcessor
import com.vanillasource.eliot.eliotc.resolve.fact.{Expression, ResolvedValue}
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerAbort

import scala.annotation.tailrec

class OperatorResolverProcessor
    extends TransformationProcessor[ResolvedValue.Key, OperatorResolvedValue.Key](key => ResolvedValue.Key(key.vfqn)) {

  override protected def generateFromKeyAndFact(
      key: OperatorResolvedValue.Key,
      resolvedValue: ResolvedValue
  ): CompilerIO[OperatorResolvedValue] =
    for {
      resolvedRuntime <- resolvedValue.runtime.traverse(expr => resolveInExpression(expr.value).map(expr.as))
    } yield OperatorResolvedValue(
      resolvedValue.vfqn,
      resolvedValue.name,
      resolvedRuntime,
      resolvedValue.typeStack,
      resolvedValue.paramConstraints
    )

  private def resolveInExpression(expr: Expression): CompilerIO[Expression] =
    expr match {
      case Expression.FlatExpression(parts)                       =>
        for {
          resolvedParts <- parts.traverse(resolveInTypeStack)
          result        <- resolveFlatExpression(resolvedParts)
        } yield result
      case Expression.FunctionApplication(target, arg)            =>
        for {
          resolvedTarget <- resolveInTypeStack(target)
          resolvedArg    <- resolveInTypeStack(arg)
        } yield Expression.FunctionApplication(resolvedTarget, resolvedArg)
      case Expression.FunctionLiteral(paramName, paramType, body) =>
        resolveInTypeStack(body).map(Expression.FunctionLiteral(paramName, paramType, _))
      case _                                                      => expr.pure[CompilerIO]
    }

  private def resolveInTypeStack(
      stack: Sourced[TypeStack[Expression]]
  ): CompilerIO[Sourced[TypeStack[Expression]]] =
    stack.value.levels.traverse(resolveInExpression).map(levels => stack.as(TypeStack(levels)))

  // --- Flat expression resolution ---

  private def resolveFlatExpression(parts: Seq[Sourced[TypeStack[Expression]]]): CompilerIO[Expression] =
    for {
      annotated <- parts.traverse(annotatePart)
      tokens     = classifyTokens(annotated)
      afterPost  = applyPostfix(tokens)
      afterPre   = applyPrefix(afterPost)
      result    <- applyInfix(afterPre)
    } yield result

  // --- Phase A: Annotate each part with its fixity ---

  private case class AnnotatedPart(part: Sourced[TypeStack[Expression]], fixity: Fixity, vfqn: Option[ValueFQN])

  private def annotatePart(part: Sourced[TypeStack[Expression]]): CompilerIO[AnnotatedPart] =
    part.value.signature match {
      case Expression.ValueReference(vfqnSrc, _) =>
        for {
          resolved <- getFactOrAbort(ResolvedValue.Key(vfqnSrc.value))
        } yield AnnotatedPart(part, resolved.fixity, Some(vfqnSrc.value))
      case _                                     => AnnotatedPart(part, Fixity.Application, None).pure[CompilerIO]
    }

  // --- Phase B: Classify into operator/operand tokens and group consecutive operands ---

  private sealed trait Token
  private case class PrefixOp(part: Sourced[TypeStack[Expression]])  extends Token
  private case class PostfixOp(part: Sourced[TypeStack[Expression]]) extends Token
  private case class InfixOp(part: Sourced[TypeStack[Expression]], associativity: Associativity, vfqn: ValueFQN)
      extends Token
  private case class Operand(part: Sourced[TypeStack[Expression]])   extends Token

  private case class ClassifyState(
      tokens: Vector[Token],
      operandGroup: Vector[Sourced[TypeStack[Expression]]],
      expectingOperand: Boolean
  ) {
    def flushGroup: ClassifyState =
      if (operandGroup.isEmpty) this
      else copy(tokens = tokens :+ Operand(curriedApplication(operandGroup)), operandGroup = Vector.empty)

    def addToken(token: Token, nowExpecting: Boolean): ClassifyState = {
      val flushed = flushGroup
      flushed.copy(tokens = flushed.tokens :+ token, expectingOperand = nowExpecting)
    }

    def addOperand(part: Sourced[TypeStack[Expression]], nowExpecting: Boolean): ClassifyState =
      copy(operandGroup = operandGroup :+ part, expectingOperand = nowExpecting)
  }

  private def classifyTokens(parts: Seq[AnnotatedPart]): Seq[Token] =
    parts
      .foldLeft(ClassifyState(Vector.empty, Vector.empty, expectingOperand = true)) { (state, ap) =>
        if (state.expectingOperand)
          ap.fixity match {
            case Fixity.Prefix => state.addToken(PrefixOp(ap.part), nowExpecting = true)
            case _             => state.addOperand(ap.part, nowExpecting = false)
          }
        else
          ap.fixity match {
            case Fixity.Infix(assoc) => state.addToken(InfixOp(ap.part, assoc, ap.vfqn.get), nowExpecting = true)
            case Fixity.Postfix      => state.addToken(PostfixOp(ap.part), nowExpecting = false)
            case Fixity.Prefix       => state.addToken(PrefixOp(ap.part), nowExpecting = true)
            case Fixity.Application  => state.addOperand(ap.part, nowExpecting = false)
          }
      }
      .flushGroup
      .tokens

  private def curriedApplication(parts: Seq[Sourced[TypeStack[Expression]]]): Sourced[TypeStack[Expression]] =
    parts.reduceLeft { (acc, arg) =>
      outlinedStack(Seq(acc, arg), Expression.FunctionApplication(acc, arg))
    }

  // --- Phase D: Apply postfix operators (bind tightest) ---

  private def applyPostfix(tokens: Seq[Token]): Seq[Token] =
    tokens.foldLeft(Vector.empty[Token]) { (acc, token) =>
      (acc.lastOption, token) match {
        case (Some(Operand(operand)), PostfixOp(op)) =>
          acc.init :+ Operand(outlinedStack(Seq(operand, op), Expression.FunctionApplication(op, operand)))
        case _                                       => acc :+ token
      }
    }

  // --- Phase E: Apply prefix operators ---

  private def applyPrefix(tokens: Seq[Token]): Seq[Token] =
    tokens.foldRight(List.empty[Token]) { (token, acc) =>
      (token, acc) match {
        case (PrefixOp(op), Operand(operand) :: rest) =>
          Operand(outlinedStack(Seq(op, operand), Expression.FunctionApplication(op, operand))) :: rest
        case _                                        => token :: acc
      }
    }

  // --- Phase F: Apply infix operators with precedence ---

  private def applyInfix(tokens: Seq[Token]): CompilerIO[Expression] = {
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
      operands: Seq[Sourced[TypeStack[Expression]]],
      operators: Seq[InfixOp],
      order: PrecedenceOrder
  ): CompilerIO[Expression] =
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
      op: Sourced[TypeStack[Expression]],
      left: Sourced[TypeStack[Expression]],
      right: Sourced[TypeStack[Expression]]
  ): Expression = {
    val app1     = Expression.FunctionApplication(op, left)
    val combined = outlinedStack(Seq(op, left), app1)
    Expression.FunctionApplication(combined, right)
  }

  // --- Utilities ---

  private def outlinedStack(
      parts: Seq[Sourced[?]],
      expr: Expression
  ): Sourced[TypeStack[Expression]] =
    Sourced.outline(parts).as(TypeStack.of(expr))
}
