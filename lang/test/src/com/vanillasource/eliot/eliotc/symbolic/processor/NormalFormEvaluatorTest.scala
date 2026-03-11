package com.vanillasource.eliot.eliotc.symbolic.processor

import cats.data.{Chain, NonEmptySeq}
import cats.effect.IO
import com.vanillasource.eliot.eliotc.ProcessorTest
import com.vanillasource.eliot.eliotc.core.fact.{QualifiedName, Qualifier, TypeStack}
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue.*
import com.vanillasource.eliot.eliotc.eval.fact.Types.{bigIntType, stringType}
import com.vanillasource.eliot.eliotc.eval.fact.{NamedEvaluable, Value}
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, ValueFQN}
import com.vanillasource.eliot.eliotc.operator.fact.{OperatorResolvedExpression, OperatorResolvedValue}
import com.vanillasource.eliot.eliotc.pos.PositionRange
import com.vanillasource.eliot.eliotc.processor.CompilerFact
import com.vanillasource.eliot.eliotc.source.content.{SourceContent, Sourced}

import java.net.URI

class NormalFormEvaluatorTest extends ProcessorTest() {
  private val bigIntTypeVfqn = ValueFQN(testModuleName, QualifiedName("BigIntType", Qualifier.Default))
  private val bigIntTypeFact = NamedEvaluable(bigIntTypeVfqn, ConcreteValue(bigIntType))
  private val stringTypeVfqn = ValueFQN(testModuleName, QualifiedName("StringType", Qualifier.Default))
  private val stringTypeFact = NamedEvaluable(stringTypeVfqn, ConcreteValue(stringType))

  "normal form evaluator" should "report unknown parameter error at call site when evaluating value reference" in {
    val defUri     = URI.create("Definition.els")
    val defContent = SourceContent(defUri, Sourced(defUri, PositionRange.zero, "def f[A](a: A) = a"))
    val vfqn       = ValueFQN(testModuleName, QualifiedName("f", Qualifier.Default))
    val bodyExpr   = OperatorResolvedExpression.FunctionLiteral(
      Sourced(defUri, PositionRange.zero, "a"),
      Some(Sourced(defUri, PositionRange.zero, TypeStack(NonEmptySeq.of(
        OperatorResolvedExpression.ParameterReference(Sourced(defUri, PositionRange.zero, "A"))
      )))),
      Sourced(defUri, PositionRange.zero,
        OperatorResolvedExpression.ParameterReference(Sourced(defUri, PositionRange.zero, "a"))
      )
    )
    val orv        = OperatorResolvedValue(
      vfqn,
      sourced(toResolve(QualifiedName("f", Qualifier.Default))),
      Some(Sourced(defUri, PositionRange.zero, bodyExpr)),
      sourced(TypeStack(NonEmptySeq.of(OperatorResolvedExpression.IntegerLiteral(sourced(BigInt(0))))))
    )
    val expression = sourced(OperatorResolvedExpression.ValueReference(sourced(vfqn), Seq.empty))
    runEvaluateForErrors(expression, Seq(orv, defContent))
      .asserting(_.head.contentSource shouldBe file.getPath)
  }

  it should "report unknown parameter error at definition when no call site" in {
    val defUri     = URI.create("Definition.els")
    val defContent = SourceContent(defUri, Sourced(defUri, PositionRange.zero, "def f = X"))
    val bodyExpr   = OperatorResolvedExpression.ParameterReference(Sourced(defUri, PositionRange.zero, "X"))
    val body       = Sourced(defUri, PositionRange.zero, bodyExpr)
    runEvaluateForErrors(body, Seq(defContent))
      .asserting(_.head.contentSource shouldBe defUri.getPath)
  }

  private def runEvaluateForErrors(
      expression: Sourced[OperatorResolvedExpression],
      facts: Seq[CompilerFact]
  ): IO[Seq[com.vanillasource.eliot.eliotc.feedback.CompilerError]] =
    for {
      generator <- createGenerator(Seq(bigIntTypeFact, stringTypeFact) ++ facts)
      result    <- NormalFormEvaluator.evaluate(expression).run(generator).run(Chain.empty).value
    } yield result match {
      case Left(errors)                          => errors.toList
      case Right((errors, _)) if errors.nonEmpty => errors.toList
      case _                                     => throw new Exception("Expected error but evaluation succeeded")
    }
}
