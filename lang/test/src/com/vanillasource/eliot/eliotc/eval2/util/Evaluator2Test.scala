package com.vanillasource.eliot.eliotc.eval2.util

import cats.data.{Chain, NonEmptySeq}
import cats.effect.IO
import com.vanillasource.eliot.eliotc.ProcessorTest
import com.vanillasource.eliot.eliotc.core.fact.{QualifiedName, Qualifier, TypeStack}
import com.vanillasource.eliot.eliotc.eval.fact.Types.{bigIntType, stringType}
import com.vanillasource.eliot.eliotc.eval.fact.Value
import com.vanillasource.eliot.eliotc.eval2.fact.Sem
import com.vanillasource.eliot.eliotc.eval2.fact.Sem.*
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, ValueFQN}
import com.vanillasource.eliot.eliotc.operator.fact.{OperatorResolvedExpression, OperatorResolvedValue}
import com.vanillasource.eliot.eliotc.processor.CompilerFact
import com.vanillasource.eliot.eliotc.resolve.fact.{QualifiedName as ResolveQualifiedName, Qualifier as ResolveQualifier}
import com.vanillasource.eliot.eliotc.source.content.Sourced

class Evaluator2Test extends ProcessorTest() {

  "evaluator2" should "evaluate integer literal" in {
    runEval(intLit(42)).asserting(_ shouldBe Sem.Lit(Value.Direct(42, bigIntType)))
  }

  it should "evaluate string literal" in {
    runEval(strLit("hello")).asserting(_ shouldBe Sem.Lit(Value.Direct("hello", stringType)))
  }

  it should "evaluate negative integer literal" in {
    runEval(intLit(-5)).asserting(_ shouldBe Sem.Lit(Value.Direct(-5, bigIntType)))
  }

  it should "evaluate empty string literal" in {
    runEval(strLit("")).asserting(_ shouldBe Sem.Lit(Value.Direct("", stringType)))
  }

  it should "evaluate parameter reference from environment" in {
    val env = Env(Map("x" -> Sem.Lit(Value.Direct(42, bigIntType))))
    runEvalWithEnv(paramRef("x"), env).asserting(_ shouldBe Sem.Lit(Value.Direct(42, bigIntType)))
  }

  it should "evaluate unbound parameter reference as neutral" in {
    runEval(paramRef("unknown")).asserting(_ shouldBe Sem.Neut(Head.Param("unknown"), Seq.empty))
  }

  it should "evaluate function literal to Lam" in {
    val expr = typedFunLit("x", bigIntTypeRef, paramRef("x"))
    runEvalWithFacts(expr, Seq(bigIntTypeFact)).asserting {
      case Sem.Lam("x", _, Closure(_, _)) => succeed
      case other                           => fail(s"Expected Lam, got: $other")
    }
  }

  it should "beta-reduce function application" in {
    val fn   = typedFunLit("x", bigIntTypeRef, paramRef("x"))
    val expr = funApp(fn, intLit(42))
    runEvalWithFacts(expr, Seq(bigIntTypeFact))
      .asserting(_ shouldBe Sem.Lit(Value.Direct(42, bigIntType)))
  }

  it should "beta-reduce nested function application" in {
    val fn   = typedFunLit("x", bigIntTypeRef, typedFunLit("y", bigIntTypeRef, paramRef("x")))
    val expr = funApp(funApp(fn, intLit(1)), intLit(2))
    runEvalWithFacts(expr, Seq(bigIntTypeFact))
      .asserting(_ shouldBe Sem.Lit(Value.Direct(1, bigIntType)))
  }

  it should "return inner parameter on nested application" in {
    val fn   = typedFunLit("x", bigIntTypeRef, typedFunLit("y", bigIntTypeRef, paramRef("y")))
    val expr = funApp(funApp(fn, intLit(1)), intLit(2))
    runEvalWithFacts(expr, Seq(bigIntTypeFact))
      .asserting(_ shouldBe Sem.Lit(Value.Direct(2, bigIntType)))
  }

  it should "handle partial application" in {
    val fn   = typedFunLit("x", bigIntTypeRef, typedFunLit("y", bigIntTypeRef, paramRef("x")))
    val expr = funApp(fn, intLit(42))
    runEvalWithFacts(expr, Seq(bigIntTypeFact)).asserting {
      case Sem.Lam("y", _, _) => succeed
      case other               => fail(s"Expected Lam, got: $other")
    }
  }

  it should "handle shadowed parameters" in {
    val fn   = typedFunLit("x", bigIntTypeRef, typedFunLit("x", bigIntTypeRef, paramRef("x")))
    val expr = funApp(funApp(fn, intLit(1)), intLit(2))
    runEvalWithFacts(expr, Seq(bigIntTypeFact))
      .asserting(_ shouldBe Sem.Lit(Value.Direct(2, bigIntType)))
  }

  it should "evaluate value reference with runtime body" in {
    val answerFqn  = ValueFQN(testModuleName, QualifiedName("answer", Qualifier.Default))
    val answerFact = makeResolvedValue(answerFqn, Some(intLit(42)))
    runEvalWithFacts(valueRef(answerFqn), Seq(answerFact))
      .asserting(_ shouldBe Sem.Lit(Value.Direct(42, bigIntType)))
  }

  it should "evaluate value reference to function and apply" in {
    val idFqn  = ValueFQN(testModuleName, QualifiedName("identity", Qualifier.Default))
    val idBody = typedFunLit("x", bigIntTypeRef, paramRef("x"))
    val idFact = makeResolvedValue(idFqn, Some(idBody))
    val expr   = funApp(valueRef(idFqn), intLit(99))
    runEvalWithFacts(expr, Seq(idFact, bigIntTypeFact))
      .asserting(_ shouldBe Sem.Lit(Value.Direct(99, bigIntType)))
  }

  it should "return neutral for opaque value reference" in {
    val opaqueFqn  = ValueFQN(testModuleName, QualifiedName("Opaque", Qualifier.Type))
    val opaqueFact = makeResolvedValue(opaqueFqn, None)
    runEvalWithFacts(valueRef(opaqueFqn), Seq(opaqueFact))
      .asserting(_ shouldBe Sem.Neut(Head.Ref(opaqueFqn), Seq.empty))
  }

  it should "return neutral for recursive value reference" in {
    val recFqn  = ValueFQN(testModuleName, QualifiedName("rec", Qualifier.Default))
    val recFact = makeResolvedValue(recFqn, Some(valueRef(recFqn)))
    runEvalWithFacts(valueRef(recFqn), Seq(recFact))
      .asserting(_ shouldBe Sem.Neut(Head.Ref(recFqn), Seq.empty))
  }

  it should "evaluate Type reference to TypeUniv" in {
    val typeFqn = com.vanillasource.eliot.eliotc.eval.fact.Types.typeFQN
    runEval(valueRef(typeFqn)).asserting(_ shouldBe Sem.TypeUniv)
  }

  it should "accumulate spine on neutral application" in {
    val litSem   = Sem.Lit(Value.Direct(1, bigIntType))
    val paramSem = Sem.Neut(Head.Param("f"), Seq.empty)
    Evaluator2.apply(paramSem, litSem).runA(MetaState()).run(testGenerator).run(Chain.empty).value.map {
      case Right((_, sem)) => sem shouldBe Sem.Neut(Head.Param("f"), Seq(litSem))
      case Left(errors)    => fail(s"Unexpected errors: $errors")
    }
  }

  it should "apply type arguments to value reference" in {
    val polyFqn  = ValueFQN(testModuleName, QualifiedName("poly", Qualifier.Default))
    val polyBody = typedFunLit("T", typeRef, paramRef("T"))
    val polyFact = makeResolvedValue(polyFqn, Some(polyBody))
    val typeFqn  = com.vanillasource.eliot.eliotc.eval.fact.Types.typeFQN

    val expr = OperatorResolvedExpression.ValueReference(sourced(polyFqn), Seq(sourced(intLit(42))))
    runEvalWithFacts(expr, Seq(polyFact, typeRefFact))
      .asserting(_ shouldBe Sem.Lit(Value.Direct(42, bigIntType)))
  }

  it should "evaluate deeply nested beta reductions" in {
    val id   = typedFunLit("x", bigIntTypeRef, paramRef("x"))
    val expr = funApp(funApp(funApp(funApp(id, id), id), id), intLit(42))
    runEvalWithFacts(expr, Seq(bigIntTypeFact))
      .asserting(_ shouldBe Sem.Lit(Value.Direct(42, bigIntType)))
  }

  it should "correctly substitute in complex body" in {
    val fn   = typedFunLit("x", bigIntTypeRef, funApp(typedFunLit("y", bigIntTypeRef, paramRef("x")), paramRef("x")))
    val expr = funApp(fn, intLit(99))
    runEvalWithFacts(expr, Seq(bigIntTypeFact))
      .asserting(_ shouldBe Sem.Lit(Value.Direct(99, bigIntType)))
  }

  // --- Helpers ---

  private val bigIntTypeFqn  = ValueFQN(
    ModuleName(ModuleName.defaultSystemPackage, "BigInteger"),
    QualifiedName("BigInteger", Qualifier.Type)
  )
  private val bigIntTypeFact = makeResolvedValue(bigIntTypeFqn, None)

  private val typeRefVfqn = com.vanillasource.eliot.eliotc.eval.fact.Types.typeFQN
  private val typeRefFact = makeResolvedValue(typeRefVfqn, None)

  private val bigIntTypeRef: OperatorResolvedExpression = valueRef(bigIntTypeFqn)
  private val typeRef: OperatorResolvedExpression       = valueRef(typeRefVfqn)

  private def intLit(value: BigInt): OperatorResolvedExpression =
    OperatorResolvedExpression.IntegerLiteral(sourced(value))

  private def strLit(value: String): OperatorResolvedExpression =
    OperatorResolvedExpression.StringLiteral(sourced(value))

  private def paramRef(name: String): OperatorResolvedExpression =
    OperatorResolvedExpression.ParameterReference(sourced(name))

  private def valueRef(vfqn: ValueFQN): OperatorResolvedExpression =
    OperatorResolvedExpression.ValueReference(sourced(vfqn))

  private def funApp(
      target: OperatorResolvedExpression,
      arg: OperatorResolvedExpression
  ): OperatorResolvedExpression =
    OperatorResolvedExpression.FunctionApplication(sourced(target), sourced(arg))

  private def typedFunLit(
      param: String,
      paramTypeExpr: OperatorResolvedExpression,
      body: OperatorResolvedExpression
  ): OperatorResolvedExpression =
    OperatorResolvedExpression.FunctionLiteral(
      sourced(param),
      Some(sourced(TypeStack(NonEmptySeq.of(paramTypeExpr)))),
      sourced(body)
    )

  private def makeResolvedValue(
      vfqn: ValueFQN,
      runtime: Option[OperatorResolvedExpression]
  ): OperatorResolvedValue =
    OperatorResolvedValue(
      vfqn,
      sourced(ResolveQualifiedName(vfqn.name.name, ResolveQualifier.Default)),
      runtime.map(sourced),
      sourced(TypeStack(NonEmptySeq.of(valueRef(bigIntTypeFqn)))),
      Map.empty
    )

  private lazy val testGenerator: com.vanillasource.eliot.eliotc.compiler.FactGenerator = {
    import cats.effect.unsafe.implicits.global
    createGenerator(Seq.empty).unsafeRunSync()
  }

  private def runEval(expr: OperatorResolvedExpression): IO[Sem] =
    runEvalWithFacts(expr, Seq.empty)

  private def runEvalWithEnv(expr: OperatorResolvedExpression, env: Env): IO[Sem] =
    runEvalWithEnvAndFacts(expr, env, Seq.empty)

  private def runEvalWithFacts(expr: OperatorResolvedExpression, facts: Seq[CompilerFact]): IO[Sem] =
    runEvalWithEnvAndFacts(expr, Env.empty, facts)

  private def runEvalWithEnvAndFacts(
      expr: OperatorResolvedExpression,
      env: Env,
      facts: Seq[CompilerFact]
  ): IO[Sem] =
    for {
      generator <- createGenerator(facts)
      result    <- Evaluator2.eval(env, expr).runA(MetaState()).run(generator).run(Chain.empty).value
    } yield result match {
      case Right((_, sem)) => sem
      case Left(errors)    =>
        throw new Exception(s"Evaluation failed: ${errors.map(_.message).toList.mkString(", ")}")
    }
}
