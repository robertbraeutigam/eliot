package com.vanillasource.eliot.eliotc.symbolic.types

import cats.data.{Chain, NonEmptySeq}
import cats.effect.IO
import com.vanillasource.eliot.eliotc.ProcessorTest
import com.vanillasource.eliot.eliotc.core.fact.Qualifier.Type
import com.vanillasource.eliot.eliotc.core.fact.{QualifiedName, Qualifier, TypeStack}
import com.vanillasource.eliot.eliotc.eval.fact.Types.typeFQN
import com.vanillasource.eliot.eliotc.eval.fact.{ExpressionValue, NamedEvaluable, Types, Value}
import com.vanillasource.eliot.eliotc.module.fact.ModuleName.defaultSystemPackage
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, ValueFQN}
import com.vanillasource.eliot.eliotc.operator.fact.{OperatorResolvedExpression, OperatorResolvedValue}
import com.vanillasource.eliot.eliotc.processor.CompilerFact
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.symbolic.fact.SymbolicType
import com.vanillasource.eliot.eliotc.symbolic.fact.SymbolicType.*
import com.vanillasource.eliot.eliotc.symbolic.types.NormalFormEvaluator

class NormalFormEvaluatorTest extends ProcessorTest() {

  private val bigIntTypeFQN  = ValueFQN(ModuleName(defaultSystemPackage, "BigInteger"), QualifiedName("BigInteger", Qualifier.Type))
  private val stringTypeFQN  = ValueFQN(ModuleName(defaultSystemPackage, "String"), QualifiedName("String", Qualifier.Type))

  // --- Literal translation ---

  "normal form evaluator" should "evaluate integer literal to LiteralType" in {
    runEvaluate(intLit(42)).asserting(_ shouldBe LiteralType(BigInt(42), bigIntTypeFQN))
  }

  it should "evaluate string literal to LiteralType" in {
    runEvaluate(strLit("hello")).asserting(_ shouldBe LiteralType("hello", stringTypeFQN))
  }

  // --- Parameter references ---

  it should "evaluate parameter reference as type variable" in {
    runEvaluate(paramRef("x"))
      .asserting(_ shouldBe TypeVariable("x"))
  }

  // --- Value references ---

  it should "evaluate data type reference into structural type" in {
    val fqn  = vfqn("MyData", Type)
    val fact = resolvedValue(fqn, body = None)
    runEvaluate(valueRef(fqn), facts = Seq(fact))
      .asserting(_ shouldBe TypeReference(fqn))
  }

  it should "evaluate value reference with runtime body" in {
    val fqn  = vfqn("myValue")
    val fact = resolvedValue(fqn, body = Some(intLit(42)))
    runEvaluate(valueRef(fqn), facts = Seq(fact))
      .asserting(_ shouldBe LiteralType(BigInt(42), bigIntTypeFQN))
  }

  it should "reject unknown value references" in {
    val fqn = vfqn("Unknown")
    runEvaluateForErrors(sourced(valueRef(fqn)))
      .asserting(_.head.message shouldBe "Can not evaluate referenced value.")
  }

  it should "report error for recursive value reference" in {
    val fqn  = vfqn("rec")
    val fact = resolvedValue(fqn, body = Some(valueRef(fqn)))
    runEvaluateForErrors(sourced(valueRef(fqn)), facts = Seq(fact))
      .asserting(_.head.message should include("Recursive"))
  }

  it should "evaluate Type FQN reference" in {
    runEvaluate(valueRef(typeFQN))
      .asserting(_ shouldBe TypeReference(typeFQN))
  }

  // --- Function literals ---

  it should "evaluate function literal with typed parameter" in {
    runEvaluate(funLit("x", valueRef(bigIntTypeVfqn), paramRef("x")))
      .asserting(_ shouldBe TypeLambda("x", TypeReference(bigIntTypeVfqn), unsourced(TypeVariable("x"))))
  }

  it should "report error for function literal without parameter type" in {
    val expr = OperatorResolvedExpression.FunctionLiteral(sourced("x"), None, sourced(intLit(1)))
    runEvaluateForErrors(sourced(expr))
      .asserting(_.head.message should include("explicit"))
  }

  // --- Function application ---

  it should "evaluate function application" in {
    val fqn  = vfqn("f")
    val fact = resolvedValue(fqn, body = Some(funLit("x", valueRef(typeFQN), paramRef("x"))))
    runEvaluate(funApp(valueRef(fqn), intLit(42)), facts = Seq(fact))
      .asserting(
        _ shouldBe TypeApplication(
          unsourced(TypeLambda("x", TypeReference(typeFQN), unsourced(TypeVariable("x")))),
          unsourced(LiteralType(BigInt(42), bigIntTypeFQN))
        )
      )
  }

  // --- Beta reduction ---

  it should "beta-reduce inlined function applied to argument" in {
    val fFqn  = vfqn("f")
    val fFact = resolvedValue(fFqn, body = Some(funLit("x", valueRef(bigIntTypeVfqn), paramRef("x"))))
    val gFqn  = vfqn("g")
    val gFact = resolvedValue(gFqn, body = Some(funApp(valueRef(fFqn), intLit(42))))
    runEvaluate(valueRef(gFqn), facts = Seq(fFact, gFact))
      .asserting(_ shouldBe LiteralType(BigInt(42), bigIntTypeFQN))
  }

  // --- Parameter references in value bodies ---

  it should "evaluate value body with unknown parameter as type variable" in {
    val fqn      = vfqn("f")
    val bodyExpr = funLit("a", paramRef("A"), paramRef("a"))
    val fact     = OperatorResolvedValue(
      fqn,
      sourced(toResolve(QualifiedName("f", Qualifier.Default))),
      Some(sourced(bodyExpr)),
      sourced(TypeStack(NonEmptySeq.of(intLit(0))))
    )
    runEvaluate(valueRef(fqn), facts = Seq(fact))
      .asserting(_ shouldBe TypeLambda("a", TypeVariable("A"), unsourced(TypeVariable("a"))))
  }

  // --- Generic param context ---

  it should "extract generic params from type stack for inlined values" in {
    val fqn          = vfqn("identity")
    val typeStackSig = OperatorResolvedExpression.FunctionLiteral(
      sourced("A"),
      Some(sourced(TypeStack(NonEmptySeq.of(valueRef(typeFQN))))),
      sourced(intLit(0))
    )
    val fact         = OperatorResolvedValue(
      fqn,
      sourced(toResolve(fqn.name)),
      Some(sourced(paramRef("A"))),
      sourced(TypeStack(NonEmptySeq.of(typeStackSig)))
    )
    runEvaluate(valueRef(fqn), facts = Seq(fact))
      .asserting(_ shouldBe TypeVariable("A"))
  }

  private val bigIntTypeVfqn = ValueFQN(testModuleName, QualifiedName("BigIntType", Qualifier.Type))
  private val bigIntTypeFact = NamedEvaluable(bigIntTypeVfqn, ExpressionValue.ConcreteValue(Types.bigIntType))
  private val stringTypeVfqn = ValueFQN(testModuleName, QualifiedName("StringType", Qualifier.Type))
  private val stringTypeFact = NamedEvaluable(stringTypeVfqn, ExpressionValue.ConcreteValue(Types.stringType))

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
      facts: Seq[CompilerFact] = Seq.empty
  ): IO[SymbolicType] =
    for {
      generator <- createGenerator(Seq(bigIntTypeFact, stringTypeFact) ++ facts)
      result    <-
        NormalFormEvaluator.evaluate(sourced(expr)).run(generator).run(Chain.empty).value
    } yield result match {
      case Right((_, value)) => value
      case Left(errors)      => throw new Exception(s"Expected success but got errors: ${errors.toList}")
    }

  private def runEvaluateForErrors(
      expression: Sourced[OperatorResolvedExpression],
      facts: Seq[CompilerFact] = Seq.empty
  ): IO[Seq[com.vanillasource.eliot.eliotc.feedback.CompilerError]] =
    for {
      generator <- createGenerator(Seq(bigIntTypeFact, stringTypeFact) ++ facts)
      result    <-
        NormalFormEvaluator.evaluate(expression).run(generator).run(Chain.empty).value
    } yield result match {
      case Left(errors)                          => errors.toList
      case Right((errors, _)) if errors.nonEmpty => errors.toList
      case _                                     => throw new Exception("Expected error but evaluation succeeded")
    }

}
