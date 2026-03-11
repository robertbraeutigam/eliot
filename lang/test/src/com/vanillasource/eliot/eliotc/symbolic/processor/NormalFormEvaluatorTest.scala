package com.vanillasource.eliot.eliotc.symbolic.processor

import cats.data.{Chain, NonEmptySeq}
import cats.effect.IO
import com.vanillasource.eliot.eliotc.ProcessorTest
import com.vanillasource.eliot.eliotc.core.fact.{QualifiedName, Qualifier, TypeStack}
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue.*
import com.vanillasource.eliot.eliotc.eval.fact.Types.{bigIntType, stringType, typeFQN}
import com.vanillasource.eliot.eliotc.eval.fact.{ExpressionValue, NamedEvaluable, Types, Value}
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

  // --- Expression DSL ---

  private def intLit(value: BigInt): OperatorResolvedExpression =
    OperatorResolvedExpression.IntegerLiteral(sourced(value))

  private def strLit(value: String): OperatorResolvedExpression =
    OperatorResolvedExpression.StringLiteral(sourced(value))

  private def paramRef(name: String): OperatorResolvedExpression =
    OperatorResolvedExpression.ParameterReference(sourced(name))

  private def valueRef(vfqn: ValueFQN): OperatorResolvedExpression =
    OperatorResolvedExpression.ValueReference(sourced(vfqn), Seq.empty)

  private def funLit(
      param: String,
      paramTypeExpr: OperatorResolvedExpression,
      body: OperatorResolvedExpression
  ): OperatorResolvedExpression =
    OperatorResolvedExpression.FunctionLiteral(
      sourced(param),
      Some(sourced(TypeStack(NonEmptySeq.of(paramTypeExpr)))),
      sourced(body)
    )

  private def funApp(target: OperatorResolvedExpression, arg: OperatorResolvedExpression): OperatorResolvedExpression =
    OperatorResolvedExpression.FunctionApplication(sourced(target), sourced(arg))

  // --- Shared fixtures ---

  private def vfqn(name: String, qualifier: Qualifier = Qualifier.Default): ValueFQN =
    ValueFQN(testModuleName, QualifiedName(name, qualifier))

  private def resolvedValue(
      valueFqn: ValueFQN,
      body: Option[OperatorResolvedExpression],
      typeStackSignature: OperatorResolvedExpression = intLit(0)
  ): OperatorResolvedValue =
    OperatorResolvedValue(
      valueFqn,
      sourced(toResolve(valueFqn.name)),
      body.map(sourced(_)),
      sourced(TypeStack(NonEmptySeq.of(typeStackSignature)))
    )

  // --- Test runners ---

  private def runEvaluate(
      expr: OperatorResolvedExpression,
      facts: Seq[CompilerFact] = Seq.empty,
      paramContext: Map[String, Value] = Map.empty
  ): IO[ExpressionValue] =
    for {
      generator <- createGenerator(Seq(bigIntTypeFact, stringTypeFact) ++ facts)
      result    <-
        NormalFormEvaluator.evaluate(sourced(expr), paramContext = paramContext).run(generator).run(Chain.empty).value
    } yield result match {
      case Right((_, value)) => value
      case Left(errors)      => throw new Exception(s"Expected success but got errors: ${errors.toList}")
    }

  private def runEvaluateForErrors(
      expression: Sourced[OperatorResolvedExpression],
      facts: Seq[CompilerFact] = Seq.empty,
      paramContext: Map[String, Value] = Map.empty
  ): IO[Seq[com.vanillasource.eliot.eliotc.feedback.CompilerError]] =
    for {
      generator <- createGenerator(Seq(bigIntTypeFact, stringTypeFact) ++ facts)
      result    <-
        NormalFormEvaluator.evaluate(expression, paramContext = paramContext).run(generator).run(Chain.empty).value
    } yield result match {
      case Left(errors)                          => errors.toList
      case Right((errors, _)) if errors.nonEmpty => errors.toList
      case _                                     => throw new Exception("Expected error but evaluation succeeded")
    }

  // --- Literal translation ---

  "normal form evaluator" should "evaluate integer literal to ConcreteValue" in {
    runEvaluate(intLit(42)).asserting(_ shouldBe ConcreteValue(Value.Direct(BigInt(42), bigIntType)))
  }

  it should "evaluate string literal to ConcreteValue" in {
    runEvaluate(strLit("hello")).asserting(_ shouldBe ConcreteValue(Value.Direct("hello", stringType)))
  }

  // --- Parameter references ---

  it should "evaluate parameter reference with known param" in {
    runEvaluate(paramRef("x"), paramContext = Map("x" -> bigIntType))
      .asserting(_ shouldBe ParameterReference("x", bigIntType))
  }

  it should "report error for unknown parameter" in {
    runEvaluateForErrors(sourced(paramRef("x")))
      .asserting(_.head.message should include("Unknown parameter"))
  }

  // --- Value references ---

  it should "evaluate data type reference with no runtime body" in {
    val fqn  = vfqn("MyData")
    val fact = resolvedValue(fqn, body = None)
    runEvaluate(valueRef(fqn), facts = Seq(fact))
      .asserting(_ shouldBe ConcreteValue(Types.dataType(fqn)))
  }

  it should "evaluate value reference with runtime body" in {
    val fqn  = vfqn("myValue")
    val fact = resolvedValue(fqn, body = Some(intLit(42)))
    runEvaluate(valueRef(fqn), facts = Seq(fact))
      .asserting(_ shouldBe ConcreteValue(Value.Direct(BigInt(42), bigIntType)))
  }

  it should "evaluate unknown value reference as data type" in {
    val fqn = vfqn("Unknown")
    runEvaluate(valueRef(fqn))
      .asserting(_ shouldBe ConcreteValue(Types.dataType(fqn)))
  }

  it should "report error for recursive value reference" in {
    val fqn  = vfqn("rec")
    val fact = resolvedValue(fqn, body = Some(valueRef(fqn)))
    runEvaluateForErrors(sourced(valueRef(fqn)), facts = Seq(fact))
      .asserting(_.head.message should include("Recursive"))
  }

  it should "evaluate Type FQN reference" in {
    runEvaluate(valueRef(typeFQN))
      .asserting(_ shouldBe ConcreteValue(Types.dataType(typeFQN)))
  }

  // --- Function literals ---

  it should "evaluate function literal with typed parameter" in {
    runEvaluate(funLit("x", valueRef(bigIntTypeVfqn), paramRef("x")))
      .asserting(_ shouldBe FunctionLiteral("x", bigIntType, unsourced(ParameterReference("x", bigIntType))))
  }

  it should "report error for function literal without parameter type" in {
    val expr = OperatorResolvedExpression.FunctionLiteral(sourced("x"), None, sourced(intLit(1)))
    runEvaluateForErrors(sourced(expr))
      .asserting(_.head.message should include("explicit"))
  }

  // --- Function application ---

  it should "evaluate function application" in {
    val fqn  = vfqn("f")
    val fact = resolvedValue(fqn, body = None)
    runEvaluate(funApp(valueRef(fqn), intLit(42)), facts = Seq(fact))
      .asserting(_ shouldBe FunctionApplication(
        unsourced(ConcreteValue(Types.dataType(fqn))),
        unsourced(ConcreteValue(Value.Direct(BigInt(42), bigIntType)))
      ))
  }

  // --- Beta reduction ---

  it should "beta-reduce inlined function applied to argument" in {
    val fFqn  = vfqn("f")
    val fFact = resolvedValue(fFqn, body = Some(funLit("x", valueRef(bigIntTypeVfqn), paramRef("x"))))
    val gFqn  = vfqn("g")
    val gFact = resolvedValue(gFqn, body = Some(funApp(valueRef(fFqn), intLit(42))))
    runEvaluate(valueRef(gFqn), facts = Seq(fFact, gFact))
      .asserting(_ shouldBe ConcreteValue(Value.Direct(BigInt(42), bigIntType)))
  }

  // --- Error source locations ---

  it should "report unknown parameter error at call site when evaluating value reference" in {
    val defUri     = URI.create("Definition.els")
    val defContent = SourceContent(defUri, Sourced(defUri, PositionRange.zero, "def f[A](a: A) = a"))
    val fqn        = vfqn("f")
    val bodyExpr   = OperatorResolvedExpression.FunctionLiteral(
      Sourced(defUri, PositionRange.zero, "a"),
      Some(
        Sourced(
          defUri,
          PositionRange.zero,
          TypeStack(
            NonEmptySeq.of(
              OperatorResolvedExpression.ParameterReference(Sourced(defUri, PositionRange.zero, "A"))
            )
          )
        )
      ),
      Sourced(
        defUri,
        PositionRange.zero,
        OperatorResolvedExpression.ParameterReference(Sourced(defUri, PositionRange.zero, "a"))
      )
    )
    val orv = OperatorResolvedValue(
      fqn,
      sourced(toResolve(QualifiedName("f", Qualifier.Default))),
      Some(Sourced(defUri, PositionRange.zero, bodyExpr)),
      sourced(TypeStack(NonEmptySeq.of(OperatorResolvedExpression.IntegerLiteral(sourced(BigInt(0))))))
    )
    val expression = sourced(OperatorResolvedExpression.ValueReference(sourced(fqn), Seq.empty))
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

  // --- Generic param context ---

  it should "extract generic params from type stack for inlined values" in {
    val fqn          = vfqn("identity")
    val typeStackSig = OperatorResolvedExpression.FunctionLiteral(
      sourced("A"),
      Some(sourced(TypeStack(NonEmptySeq.of(valueRef(typeFQN))))),
      sourced(intLit(0))
    )
    val fact = OperatorResolvedValue(
      fqn,
      sourced(toResolve(fqn.name)),
      Some(sourced(paramRef("A"))),
      sourced(TypeStack(NonEmptySeq.of(typeStackSig)))
    )
    runEvaluate(valueRef(fqn), facts = Seq(fact))
      .asserting(_ shouldBe ParameterReference("A", Value.Type))
  }
}
