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

import scala.collection.mutable

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
      grouped    = groupOperands(tokens)
      afterPost  = applyPostfix(grouped)
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

  // --- Phase B: Classify into operator/operand tokens ---

  private sealed trait Token
  private case class PrefixOp(part: Sourced[TypeStack[Expression]])  extends Token
  private case class PostfixOp(part: Sourced[TypeStack[Expression]]) extends Token
  private case class InfixOp(part: Sourced[TypeStack[Expression]], associativity: Associativity, vfqn: ValueFQN)
      extends Token
  private case class Operand(part: Sourced[TypeStack[Expression]])   extends Token

  private def classifyTokens(parts: Seq[AnnotatedPart]): Seq[Token] = {
    var expectingOperand = true
    val tokens           = Seq.newBuilder[Token]
    val operandGroup     = Seq.newBuilder[Sourced[TypeStack[Expression]]]
    var hasOperandGroup  = false

    def flushOperandGroup(): Unit =
      if (hasOperandGroup) {
        val group = operandGroup.result()
        group.foreach(p => tokens += Operand(p))
        operandGroup.clear()
        hasOperandGroup = false
      }

    for (ap <- parts) {
      if (expectingOperand) {
        ap.fixity match {
          case Fixity.Prefix =>
            flushOperandGroup()
            tokens += PrefixOp(ap.part)
          case _             =>
            operandGroup += ap.part
            hasOperandGroup = true
            expectingOperand = false
        }
      } else {
        ap.fixity match {
          case Fixity.Infix(assoc) =>
            flushOperandGroup()
            tokens += InfixOp(ap.part, assoc, ap.vfqn.get)
            expectingOperand = true
          case Fixity.Postfix      =>
            flushOperandGroup()
            tokens += PostfixOp(ap.part)
          case Fixity.Prefix       =>
            flushOperandGroup()
            tokens += PrefixOp(ap.part)
            expectingOperand = true
          case Fixity.Application  =>
            operandGroup += ap.part
            hasOperandGroup = true
        }
      }
    }
    flushOperandGroup()
    tokens.result()
  }

  // --- Phase C: Group consecutive operands into curried applications ---

  private def groupOperands(tokens: Seq[Token]): Seq[Token] = {
    val result        = Seq.newBuilder[Token]
    val group         = Seq.newBuilder[Sourced[TypeStack[Expression]]]
    var groupNonEmpty = false

    def flushGroup(): Unit =
      if (groupNonEmpty) {
        val parts    = group.result()
        val combined = curriedApplication(parts)
        result += Operand(combined)
        group.clear()
        groupNonEmpty = false
      }

    for (token <- tokens) {
      token match {
        case Operand(part) =>
          group += part
          groupNonEmpty = true
        case other         =>
          flushGroup()
          result += other
      }
    }
    flushGroup()
    result.result()
  }

  private def curriedApplication(parts: Seq[Sourced[TypeStack[Expression]]]): Sourced[TypeStack[Expression]] =
    parts.reduceLeft { (acc, arg) =>
      outlinedStack(Seq(acc, arg), Expression.FunctionApplication(acc, arg))
    }

  // --- Phase D: Apply postfix operators (bind tightest) ---

  private def applyPostfix(tokens: Seq[Token]): Seq[Token] = {
    var result  = tokens
    var changed = true
    while (changed) {
      changed = false
      val newResult = Seq.newBuilder[Token]
      var i         = 0
      while (i < result.length) {
        if (i + 1 < result.length) {
          (result(i), result(i + 1)) match {
            case (Operand(operand), PostfixOp(op)) =>
              newResult += Operand(outlinedStack(Seq(operand, op), Expression.FunctionApplication(op, operand)))
              i += 2
              changed = true
            case _                                 =>
              newResult += result(i)
              i += 1
          }
        } else {
          newResult += result(i)
          i += 1
        }
      }
      result = newResult.result()
    }
    result
  }

  // --- Phase E: Apply prefix operators ---

  private def applyPrefix(tokens: Seq[Token]): Seq[Token] = {
    var result  = tokens
    var changed = true
    while (changed) {
      changed = false
      val newResult    = Seq.newBuilder[Token]
      var i            = result.length - 1
      val tempReversed = mutable.ArrayDeque.empty[Token]
      while (i >= 0) {
        if (i >= 1) {
          (result(i - 1), result(i)) match {
            case (PrefixOp(op), Operand(operand)) =>
              tempReversed.prepend(
                Operand(outlinedStack(Seq(op, operand), Expression.FunctionApplication(op, operand)))
              )
              i -= 2
              changed = true
            case _                                =>
              tempReversed.prepend(result(i))
              i -= 1
          }
        } else {
          tempReversed.prepend(result(i))
          i -= 1
        }
      }
      result = tempReversed.toSeq
    }
    result
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
      val visited  = mutable.Set.empty[ValueFQN]
      val queue    = mutable.Queue.from(sameAs.getOrElse(a, Set.empty) + a)

      while (queue.nonEmpty) {
        val current = queue.dequeue()
        if (visited.add(current)) {
          val lower = higherThan.getOrElse(current, Set.empty)
          if (lower.exists(bTargets.contains)) return true
          lower.filterNot(visited.contains).foreach { l =>
            queue.enqueue(l)
            queue.enqueueAll(sameAs.getOrElse(l, Set.empty).filterNot(visited.contains))
          }
        }
      }
      false
    }
  }

  private def buildPrecedenceOrder(operators: Seq[InfixOp]): CompilerIO[PrecedenceOrder] = {
    val vfqns = operators.map(_.vfqn).distinct
    for {
      rvs <- vfqns.traverse(v => getFactOrAbort(ResolvedValue.Key(v)).map(v -> _))
    } yield {
      val higherThan = mutable.Map.empty[ValueFQN, Set[ValueFQN]]
      val sameAs     = mutable.Map.empty[ValueFQN, Set[ValueFQN]]

      for ((vfqn, rv) <- rvs; decl <- rv.precedence) {
        val targets = decl.targets.map(_.value)
        decl.relation match {
          case Relation.Above =>
            higherThan(vfqn) = higherThan.getOrElse(vfqn, Set.empty) ++ targets
          case Relation.Below =>
            targets.foreach(t => higherThan(t) = higherThan.getOrElse(t, Set.empty) + vfqn)
          case Relation.At    =>
            targets.foreach { t =>
              sameAs(vfqn) = sameAs.getOrElse(vfqn, Set.empty) + t
              sameAs(t) = sameAs.getOrElse(t, Set.empty) + vfqn
            }
        }
      }

      PrecedenceOrder(higherThan.toMap, sameAs.toMap)
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

  private def findSplitPoint(operators: Seq[InfixOp], order: PrecedenceOrder): CompilerIO[Int] = {
    // Find the weakest precedence level, then pick split point by associativity
    var weakestIdx                            = 0
    var compareError: Option[CompilerIO[Int]] = None

    for (i <- 1 until operators.length) {
      if (compareError.isEmpty) {
        val cmp = order.compare(operators(i).vfqn, operators(weakestIdx).vfqn)
        cmp match {
          case Some(c) if c < 0 => weakestIdx = i
          case Some(_)          => // same or higher, keep current weakest
          case None             =>
            compareError = Some(
              compilerAbort(
                operators(i).part.as(
                  s"Operators '${operators(i).vfqn.name.name}' and '${operators(weakestIdx).vfqn.name.name}' have no defined relative precedence."
                )
              )
            )
        }
      }
    }

    compareError.getOrElse {
      val weakestVfqn      = operators(weakestIdx).vfqn
      val sameLevelIndices = operators.indices.filter { i =>
        order.compare(operators(i).vfqn, weakestVfqn).contains(0)
      }

      if (sameLevelIndices.length == 1) {
        sameLevelIndices.head.pure[CompilerIO]
      } else {
        val associativities = sameLevelIndices.map(i => operators(i).associativity).distinct
        if (associativities.length > 1) {
          compilerAbort(
            operators(sameLevelIndices.head).part.as(
              "Operators at same precedence level have different associativity."
            )
          )
        } else {
          associativities.head match {
            case Associativity.Left  => sameLevelIndices.last.pure[CompilerIO]
            case Associativity.Right => sameLevelIndices.head.pure[CompilerIO]
            case Associativity.None  =>
              compilerAbort(
                operators(sameLevelIndices.head).part.as(
                  s"Non-associative operator '${operators(sameLevelIndices.head).vfqn.name.name}' cannot be chained."
                )
              )
          }
        }
      }
    }
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
