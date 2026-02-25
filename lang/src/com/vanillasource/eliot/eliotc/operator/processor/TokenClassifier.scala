package com.vanillasource.eliot.eliotc.operator.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.ast.fact.Fixity
import com.vanillasource.eliot.eliotc.ast.fact.Fixity.Associativity
import com.vanillasource.eliot.eliotc.core.fact.TypeStack
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedExpression
import com.vanillasource.eliot.eliotc.source.content.Sourced

object TokenClassifier {
  case class AnnotatedPart(part: Sourced[TypeStack[OperatorResolvedExpression]], fixity: Fixity, vfqn: Option[ValueFQN])

  sealed trait Token
  case class PrefixOp(part: Sourced[TypeStack[OperatorResolvedExpression]])                                            extends Token
  case class PostfixOp(part: Sourced[TypeStack[OperatorResolvedExpression]])                                           extends Token
  case class InfixOp(part: Sourced[TypeStack[OperatorResolvedExpression]], associativity: Associativity, vfqn: ValueFQN) extends Token
  case class Operand(part: Sourced[TypeStack[OperatorResolvedExpression]])                                             extends Token

  private case class ClassifyState(
      tokens: Vector[Token],
      operandGroup: Vector[Sourced[TypeStack[OperatorResolvedExpression]]],
      expectingOperand: Boolean
  ) {
    def flushGroup: ClassifyState =
      if (operandGroup.isEmpty) this
      else copy(tokens = tokens :+ Operand(curriedApplication(operandGroup)), operandGroup = Vector.empty)

    def addToken(token: Token, nowExpecting: Boolean): ClassifyState = {
      val flushed = flushGroup
      flushed.copy(tokens = flushed.tokens :+ token, expectingOperand = nowExpecting)
    }

    def addOperand(part: Sourced[TypeStack[OperatorResolvedExpression]], nowExpecting: Boolean): ClassifyState =
      copy(operandGroup = operandGroup :+ part, expectingOperand = nowExpecting)
  }

  def classifyTokens(parts: Seq[AnnotatedPart]): Seq[Token] =
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

  def applyPostfix(tokens: Seq[Token]): Seq[Token] =
    tokens.foldLeft(Vector.empty[Token]) { (acc, token) =>
      (acc.lastOption, token) match {
        case (Some(Operand(operand)), PostfixOp(op)) =>
          acc.init :+ Operand(outlinedStack(Seq(operand, op), OperatorResolvedExpression.FunctionApplication(op, operand)))
        case _                                       => acc :+ token
      }
    }

  def applyPrefix(tokens: Seq[Token]): Seq[Token] =
    tokens.foldRight(List.empty[Token]) { (token, acc) =>
      (token, acc) match {
        case (PrefixOp(op), Operand(operand) :: rest) =>
          Operand(outlinedStack(Seq(op, operand), OperatorResolvedExpression.FunctionApplication(op, operand))) :: rest
        case _                                        => token :: acc
      }
    }

  def curriedApplication(
      parts: Seq[Sourced[TypeStack[OperatorResolvedExpression]]]
  ): Sourced[TypeStack[OperatorResolvedExpression]] =
    parts.reduceLeft { (acc, arg) =>
      outlinedStack(Seq(acc, arg), OperatorResolvedExpression.FunctionApplication(acc, arg))
    }

  def outlinedStack(
      parts: Seq[Sourced[?]],
      expr: OperatorResolvedExpression
  ): Sourced[TypeStack[OperatorResolvedExpression]] =
    Sourced.outline(parts).as(TypeStack.of(expr))
}
