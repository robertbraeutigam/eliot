package com.vanillasource.eliot.eliotc.eval2.util

import cats.data.{Chain, NonEmptySeq}
import cats.effect.IO
import com.vanillasource.eliot.eliotc.ProcessorTest
import com.vanillasource.eliot.eliotc.core.fact.{QualifiedName, Qualifier, TypeStack}
import com.vanillasource.eliot.eliotc.eval.fact.Types.{bigIntType, stringType}
import com.vanillasource.eliot.eliotc.eval.fact.Value
import com.vanillasource.eliot.eliotc.eval2.fact.*
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, ValueFQN}
import com.vanillasource.eliot.eliotc.operator.fact.{OperatorResolvedExpression, OperatorResolvedValue}
import com.vanillasource.eliot.eliotc.processor.CompilerFact
import com.vanillasource.eliot.eliotc.resolve.fact.{QualifiedName as RQualifiedName, Qualifier as RQualifier}
import com.vanillasource.eliot.eliotc.source.content.Sourced

class Evaluator2Test extends ProcessorTest() {

  "evaluator2" should "evaluate integer literal" in {
    runEval(intLit(42)).asserting(_ shouldBe Sem.Lit(Value.Direct(42, bigIntType)))
  }

  it should "evaluate string literal" in {
    runEval(strLit("hello")).asserting(_ shouldBe Sem.Lit(Value.Direct("hello", stringType)))
  }

  it should "evaluate unbound parameter reference to neutral" in {
    runEval(paramRef("x")).asserting(_ shouldBe Sem.Neut(Head.Param("x"), Seq.empty))
  }

  it should "evaluate bound parameter reference from environment" in {
    val env = Env.empty.extend("x", Sem.Lit(Value.Direct(42, bigIntType)))
    runEvalInEnv(env, paramRef("x")).asserting(_ shouldBe Sem.Lit(Value.Direct(42, bigIntType)))
  }

  it should "evaluate function literal to lambda" in {
    runEval(funLit("x", intTypeRef, paramRef("x"))).asserting {
      case Sem.Lam("x", _, Closure(_, _)) => succeed
      case other                          => fail(s"Expected Lam, got: $other")
    }
  }

  it should "evaluate unannotated function literal" in {
    runEval(unannotatedFunLit("x", paramRef("x"))).asserting {
      case Sem.Lam("x", Sem.TypeUniv, _) => succeed
      case other                          => fail(s"Expected Lam with TypeUniv dom, got: $other")
    }
  }

  it should "beta-reduce function application" in {
    val fn   = funLit("x", intTypeRef, paramRef("x"))
    val expr = funApp(fn, intLit(42))
    runEval(expr).asserting(_ shouldBe Sem.Lit(Value.Direct(42, bigIntType)))
  }

  it should "beta-reduce nested function applications" in {
    val fn   = funLit("x", intTypeRef, funLit("y", intTypeRef, paramRef("x")))
    val expr = funApp(funApp(fn, intLit(1)), intLit(2))
    runEval(expr).asserting(_ shouldBe Sem.Lit(Value.Direct(1, bigIntType)))
  }

  it should "return inner parameter in nested application" in {
    val fn   = funLit("x", intTypeRef, funLit("y", intTypeRef, paramRef("y")))
    val expr = funApp(funApp(fn, intLit(1)), intLit(2))
    runEval(expr).asserting(_ shouldBe Sem.Lit(Value.Direct(2, bigIntType)))
  }

  it should "handle partial application" in {
    val fn   = funLit("x", intTypeRef, funLit("y", intTypeRef, paramRef("x")))
    val expr = funApp(fn, intLit(42))
    runEval(expr).asserting {
      case Sem.Lam("y", _, Closure(env, _)) =>
        env.params.get("x") shouldBe Some(Sem.Lit(Value.Direct(42, bigIntType)))
      case other                            => fail(s"Expected Lam, got: $other")
    }
  }

  it should "handle shadowed parameters" in {
    val fn   = funLit("x", intTypeRef, funLit("x", intTypeRef, paramRef("x")))
    val expr = funApp(funApp(fn, intLit(1)), intLit(2))
    runEval(expr).asserting(_ shouldBe Sem.Lit(Value.Direct(2, bigIntType)))
  }

  it should "apply neutral term to argument" in {
    val expr = funApp(paramRef("f"), intLit(42))
    runEval(expr).asserting(_ shouldBe Sem.Neut(Head.Param("f"), Seq(Sem.Lit(Value.Direct(42, bigIntType)))))
  }

  it should "accumulate spine for multiple applications to neutral" in {
    val expr = funApp(funApp(paramRef("f"), intLit(1)), intLit(2))
    runEval(expr).asserting(
      _ shouldBe Sem.Neut(
        Head.Param("f"),
        Seq(Sem.Lit(Value.Direct(1, bigIntType)), Sem.Lit(Value.Direct(2, bigIntType)))
      )
    )
  }

  it should "resolve value reference with runtime body" in {
    val vfqn    = ValueFQN(testModuleName, QualifiedName("zero", Qualifier.Default))
    val orv     = makeORV(vfqn, Some(intLit(0)), intTypeRef)
    runEvalWithFacts(valueRef(vfqn), Seq(orv)).asserting(
      _ shouldBe Sem.Lit(Value.Direct(BigInt(0), bigIntType))
    )
  }

  it should "resolve value reference without runtime to neutral" in {
    val vfqn = ValueFQN(testModuleName, QualifiedName("opaque_val", Qualifier.Default))
    val orv  = makeORV(vfqn, None, intTypeRef)
    runEvalWithFacts(valueRef(vfqn), Seq(orv)).asserting(
      _ shouldBe Sem.Neut(Head.Ref(vfqn), Seq.empty)
    )
  }

  it should "detect direct recursion and return neutral" in {
    val vfqn = ValueFQN(testModuleName, QualifiedName("rec", Qualifier.Default))
    val orv  = makeORV(vfqn, Some(valueRef(vfqn)), intTypeRef)
    runEvalWithFacts(valueRef(vfqn), Seq(orv)).asserting(
      _ shouldBe Sem.Neut(Head.Ref(vfqn), Seq.empty)
    )
  }

  it should "detect mutual recursion and return neutral" in {
    val vfqnA = ValueFQN(testModuleName, QualifiedName("a", Qualifier.Default))
    val vfqnB = ValueFQN(testModuleName, QualifiedName("b", Qualifier.Default))
    val orvA  = makeORV(vfqnA, Some(valueRef(vfqnB)), intTypeRef)
    val orvB  = makeORV(vfqnB, Some(valueRef(vfqnA)), intTypeRef)
    runEvalWithFacts(valueRef(vfqnA), Seq(orvA, orvB)).asserting(
      _ shouldBe Sem.Neut(Head.Ref(vfqnA), Seq.empty)
    )
  }

  it should "resolve function value reference and apply" in {
    val vfqn  = ValueFQN(testModuleName, QualifiedName("identity", Qualifier.Default))
    val idFn  = funLit("x", intTypeRef, paramRef("x"))
    val orv   = makeORV(vfqn, Some(idFn), intTypeRef)
    val expr  = funApp(valueRef(vfqn), intLit(42))
    runEvalWithFacts(expr, Seq(orv)).asserting(
      _ shouldBe Sem.Lit(Value.Direct(42, bigIntType))
    )
  }

  it should "apply type arguments to value reference" in {
    val vfqn    = ValueFQN(testModuleName, QualifiedName("poly", Qualifier.Default))
    val polyFn  = funLit("A", typeTypeRef, funLit("a", paramRef("A"), paramRef("a")))
    val orv     = makeORV(vfqn, Some(polyFn), typeTypeRef)
    val expr    = valueRefWithTypeArgs(vfqn, Seq(intTypeRef))
    runEvalWithFacts(expr, Seq(orv, intTypeORV)).asserting {
      case Sem.Lam("a", _, _) => succeed
      case other              => fail(s"Expected Lam after type arg application, got: $other")
    }
  }

  it should "reduce complex nested expression" in {
    val const = funLit("x", intTypeRef, funLit("y", intTypeRef, paramRef("x")))
    val expr  = funApp(funApp(const, intLit(1)), intLit(2))
    runEval(expr).asserting(_ shouldBe Sem.Lit(Value.Direct(1, bigIntType)))
  }

  it should "evaluate deeply nested function applications" in {
    val id   = funLit("x", intTypeRef, paramRef("x"))
    val expr = funApp(funApp(funApp(funApp(id, id), id), id), intLit(42))
    runEval(expr).asserting(_ shouldBe Sem.Lit(Value.Direct(42, bigIntType)))
  }

  it should "correctly substitute in complex body" in {
    val fn   = funLit("x", intTypeRef, funApp(funLit("y", intTypeRef, paramRef("x")), paramRef("x")))
    val expr = funApp(fn, intLit(99))
    runEval(expr).asserting(_ shouldBe Sem.Lit(Value.Direct(99, bigIntType)))
  }

  // --- Helpers ---

  private val intTypeFQN    = ValueFQN(
    ModuleName(Seq("eliot", "lang"), "BigInteger"),
    QualifiedName("BigInteger", Qualifier.Type)
  )
  private val stringTypeFQN = ValueFQN(
    ModuleName(Seq("eliot", "lang"), "String"),
    QualifiedName("String", Qualifier.Type)
  )
  private val typeTypeFQN   = ValueFQN(
    ModuleName(Seq("eliot", "lang"), "Type"),
    QualifiedName("Type", Qualifier.Type)
  )
  private val intTypeRef    = valueRef(intTypeFQN)
  private val typeTypeRef   = valueRef(typeTypeFQN)
  private val intTypeORV    = makeORV(intTypeFQN, None, typeTypeRef)
  private val stringTypeORV = makeORV(stringTypeFQN, None, typeTypeRef)
  private val baseFacts: Seq[CompilerFact] = Seq(intTypeORV, stringTypeORV)

  private def intLit(value: BigInt): OperatorResolvedExpression =
    OperatorResolvedExpression.IntegerLiteral(sourced(value))

  private def strLit(value: String): OperatorResolvedExpression =
    OperatorResolvedExpression.StringLiteral(sourced(value))

  private def paramRef(name: String): OperatorResolvedExpression =
    OperatorResolvedExpression.ParameterReference(sourced(name))

  private def valueRef(vfqn: ValueFQN): OperatorResolvedExpression =
    OperatorResolvedExpression.ValueReference(sourced(vfqn))

  private def valueRefWithTypeArgs(
      vfqn: ValueFQN,
      typeArgs: Seq[OperatorResolvedExpression]
  ): OperatorResolvedExpression =
    OperatorResolvedExpression.ValueReference(sourced(vfqn), typeArgs.map(sourced))

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

  private def unannotatedFunLit(
      param: String,
      body: OperatorResolvedExpression
  ): OperatorResolvedExpression =
    OperatorResolvedExpression.FunctionLiteral(
      sourced(param),
      None,
      sourced(body)
    )

  private def funApp(
      target: OperatorResolvedExpression,
      arg: OperatorResolvedExpression
  ): OperatorResolvedExpression =
    OperatorResolvedExpression.FunctionApplication(sourced(target), sourced(arg))

  private def makeORV(
      vfqn: ValueFQN,
      runtime: Option[OperatorResolvedExpression],
      signatureExpr: OperatorResolvedExpression
  ): OperatorResolvedValue =
    OperatorResolvedValue(
      vfqn,
      sourced(RQualifiedName(vfqn.name.name, RQualifier.Default)),
      runtime.map(sourced),
      sourced(TypeStack(NonEmptySeq.of(signatureExpr)))
    )

  private def runEval(expr: OperatorResolvedExpression): IO[Sem] =
    runEvalWithFacts(expr, Seq.empty)

  private def runEvalInEnv(env: Env, expr: OperatorResolvedExpression): IO[Sem] =
    runEvalInEnvWithFacts(env, expr, Seq.empty)

  private def runEvalWithFacts(expr: OperatorResolvedExpression, facts: Seq[CompilerFact]): IO[Sem] =
    runEvalInEnvWithFacts(Env.empty, expr, facts)

  private def runEvalInEnvWithFacts(
      env: Env,
      expr: OperatorResolvedExpression,
      facts: Seq[CompilerFact]
  ): IO[Sem] =
    for {
      generator <- createGenerator(baseFacts ++ facts)
      result    <- Evaluator2.eval(env, expr).run(MetaState()).run(generator).run(Chain.empty).value
    } yield result match {
      case Right((_, (_, sem))) => sem
      case Left(errors)         =>
        throw Exception(s"Evaluation failed: ${errors.toList.map(_.message).mkString(", ")}")
    }
}
