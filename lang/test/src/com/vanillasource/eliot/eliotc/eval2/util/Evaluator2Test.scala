package com.vanillasource.eliot.eliotc.eval2.util

import cats.data.Chain
import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import com.vanillasource.eliot.eliotc.ProcessorTest
import com.vanillasource.eliot.eliotc.core.fact.{QualifiedName, Qualifier, TypeStack}
import com.vanillasource.eliot.eliotc.eval.fact.Types.{bigIntType, stringType, typeFQN}
import com.vanillasource.eliot.eliotc.eval.fact.Value
import com.vanillasource.eliot.eliotc.eval2.fact.*
import com.vanillasource.eliot.eliotc.eval2.util.MetaState.EvalIO
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, ValueFQN}
import com.vanillasource.eliot.eliotc.operator.fact.{OperatorResolvedExpression, OperatorResolvedValue}
import com.vanillasource.eliot.eliotc.pos.PositionRange
import com.vanillasource.eliot.eliotc.processor.CompilerFact
import com.vanillasource.eliot.eliotc.source.content.Sourced
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

class Evaluator2Test extends ProcessorTest() {

  "Evaluator2" should "evaluate integer literal" in {
    runEval(intLit(42))
      .asserting(_ shouldBe Sem.Lit(Value.Direct(42, bigIntType)))
  }

  it should "evaluate string literal" in {
    runEval(strLit("hello"))
      .asserting(_ shouldBe Sem.Lit(Value.Direct("hello", stringType)))
  }

  it should "evaluate negative integer literal" in {
    runEval(intLit(-123))
      .asserting(_ shouldBe Sem.Lit(Value.Direct(-123, bigIntType)))
  }

  it should "evaluate empty string literal" in {
    runEval(strLit(""))
      .asserting(_ shouldBe Sem.Lit(Value.Direct("", stringType)))
  }

  it should "evaluate bound parameter reference from environment" in {
    val env = Env.empty.extend("x", Sem.Lit(Value.Direct(42, bigIntType)))
    runEvalInEnv(env, paramRef("x"))
      .asserting(_ shouldBe Sem.Lit(Value.Direct(42, bigIntType)))
  }

  it should "evaluate unbound parameter reference as neutral" in {
    runEval(paramRef("unknown"))
      .asserting(_ shouldBe Sem.Neut(Head.Param("unknown")))
  }

  it should "evaluate function literal to Lam" in {
    runEval(intFunLit("x", paramRef("x")))
      .asserting {
        case Sem.Lam("x", _, Closure(_, _)) => succeed
        case other                           => fail(s"Expected Lam, got $other")
      }
  }

  it should "beta-reduce function application" in {
    val fn   = intFunLit("x", paramRef("x"))
    val expr = funApp(fn, intLit(42))
    runEval(expr)
      .asserting(_ shouldBe Sem.Lit(Value.Direct(42, bigIntType)))
  }

  it should "beta-reduce nested function application" in {
    val fn   = intFunLit("x", intFunLit("y", paramRef("x")))
    val expr = funApp(funApp(fn, intLit(1)), intLit(2))
    runEval(expr)
      .asserting(_ shouldBe Sem.Lit(Value.Direct(1, bigIntType)))
  }

  it should "handle partial application" in {
    val fn   = intFunLit("x", intFunLit("y", paramRef("x")))
    val expr = funApp(fn, intLit(42))
    runEval(expr)
      .asserting {
        case Sem.Lam("y", _, _) => succeed
        case other               => fail(s"Expected partial Lam, got $other")
      }
  }

  it should "handle parameter shadowing" in {
    val fn   = intFunLit("x", intFunLit("x", paramRef("x")))
    val expr = funApp(funApp(fn, intLit(1)), intLit(2))
    runEval(expr)
      .asserting(_ shouldBe Sem.Lit(Value.Direct(2, bigIntType)))
  }

  it should "accumulate spine on neutral application" in {
    val expr = funApp(paramRef("f"), intLit(42))
    runEval(expr)
      .asserting {
        case Sem.Neut(Head.Param("f"), spine) =>
          spine.length shouldBe 1
          spine.head shouldBe Sem.Lit(Value.Direct(42, bigIntType))
        case other                             => fail(s"Expected Neut with spine, got $other")
      }
  }

  it should "evaluate value reference with runtime body" in {
    val vfqn = ValueFQN(testModuleName, QualifiedName("constVal", Qualifier.Default))
    val ore  = makeORV(vfqn, Some(OperatorResolvedExpression.IntegerLiteral(src(BigInt(100)))))
    runEvalWithFacts(valueRef(vfqn), defaultFacts :+ ore)
      .asserting(_ shouldBe Sem.Lit(Value.Direct(100, bigIntType)))
  }

  it should "return neutral Ref for opaque value" in {
    val vfqn = ValueFQN(testModuleName, QualifiedName("opq", Qualifier.Type))
    val ore  = makeORV(vfqn, None, opaque = true)
    runEvalWithFacts(valueRef(vfqn), defaultFacts :+ ore)
      .asserting(_ shouldBe Sem.Neut(Head.Ref(vfqn)))
  }

  it should "return neutral Ref for recursive value" in {
    import scala.concurrent.duration.*
    val vfqn = ValueFQN(testModuleName, QualifiedName("rec", Qualifier.Default))
    val ore  = makeORV(vfqn, Some(OperatorResolvedExpression.ValueReference(src(vfqn))))
    runEvalWithFacts(valueRef(vfqn), defaultFacts :+ ore)
      .timeout(2.seconds)
      .asserting(_ shouldBe Sem.Neut(Head.Ref(vfqn)))
  }

  it should "evaluate Type reference as TypeUniv" in {
    runEval(valueRef(typeFQN))
      .asserting(_ shouldBe Sem.TypeUniv)
  }

  it should "evaluate type constructor (no runtime body, signature is Type)" in {
    val vfqn = ValueFQN(testModuleName, QualifiedName("MyType", Qualifier.Type))
    val ore  = makeORV(
      vfqn,
      None,
      sig = OperatorResolvedExpression.ValueReference(src(typeFQN))
    )
    runEvalWithFacts(valueRef(vfqn), defaultFacts :+ ore)
      .asserting {
        case Sem.Struct(fqn, fields) =>
          fqn shouldBe vfqn
          fields.contains("$typeName") shouldBe true
        case other                    => fail(s"Expected Struct, got $other")
      }
  }

  it should "evaluate polymorphic value with type arguments" in {
    val vfqn      = ValueFQN(testModuleName, QualifiedName("id", Qualifier.Default))
    val bigIntFQN = com.vanillasource.eliot.eliotc.eval.fact.Types.bigIntFQN
    val body      = OperatorResolvedExpression.FunctionLiteral(
      src("A"),
      Some(src(TypeStack.of(OperatorResolvedExpression.ValueReference(src(typeFQN))))),
      src(
        OperatorResolvedExpression.FunctionLiteral(
          src("a"),
          Some(src(TypeStack.of(OperatorResolvedExpression.ParameterReference(src("A"))))),
          src(OperatorResolvedExpression.ParameterReference(src("a")))
        )
      )
    )
    val ore       = makeORV(vfqn, Some(body))
    val typeArg   = src(OperatorResolvedExpression.ValueReference(src(bigIntFQN)))
    val expr      = OperatorResolvedExpression.ValueReference(src(vfqn), Seq(typeArg))
    runEvalWithFacts(expr, defaultFacts :+ ore)
      .asserting {
        case Sem.Lam("a", _, _) => succeed
        case other               => fail(s"Expected Lam after type arg application, got $other")
      }
  }

  it should "handle complex nested beta reduction" in {
    val const = intFunLit("x", intFunLit("y", paramRef("x")))
    val expr  = funApp(funApp(const, intLit(1)), intLit(2))
    runEval(expr)
      .asserting(_ shouldBe Sem.Lit(Value.Direct(1, bigIntType)))
  }

  // --- Helpers ---

  private def src[T](v: T): Sourced[T] = Sourced(file, PositionRange.zero, v)

  private def intLit(value: BigInt): OperatorResolvedExpression =
    OperatorResolvedExpression.IntegerLiteral(src(value))

  private def strLit(value: String): OperatorResolvedExpression =
    OperatorResolvedExpression.StringLiteral(src(value))

  private def paramRef(name: String): OperatorResolvedExpression =
    OperatorResolvedExpression.ParameterReference(src(name))

  private def valueRef(vfqn: ValueFQN): OperatorResolvedExpression =
    OperatorResolvedExpression.ValueReference(src(vfqn))

  private def intFunLit(param: String, body: OperatorResolvedExpression): OperatorResolvedExpression = {
    val bigIntFQN = com.vanillasource.eliot.eliotc.eval.fact.Types.bigIntFQN
    OperatorResolvedExpression.FunctionLiteral(
      src(param),
      Some(src(TypeStack.of(OperatorResolvedExpression.ValueReference(src(bigIntFQN))))),
      src(body)
    )
  }

  private def funApp(
      target: OperatorResolvedExpression,
      arg: OperatorResolvedExpression
  ): OperatorResolvedExpression =
    OperatorResolvedExpression.FunctionApplication(src(target), src(arg))

  private def makeORV(
      vfqn: ValueFQN,
      runtime: Option[OperatorResolvedExpression],
      sig: OperatorResolvedExpression =
        OperatorResolvedExpression.ValueReference(src(com.vanillasource.eliot.eliotc.eval.fact.Types.bigIntFQN)),
      opaque: Boolean = false
  ): OperatorResolvedValue =
    OperatorResolvedValue(
      vfqn,
      src(toResolve(vfqn.name)),
      runtime.map(src(_)),
      src(TypeStack.of(sig)),
      opaque = opaque
    )

  private def makeBigIntORV(): OperatorResolvedValue = {
    val bigIntFQN = com.vanillasource.eliot.eliotc.eval.fact.Types.bigIntFQN
    makeORV(
      bigIntFQN,
      None,
      sig = OperatorResolvedExpression.ValueReference(src(typeFQN)),
      opaque = true
    )
  }

  private def makeStringORV(): OperatorResolvedValue = {
    val stringFQN = com.vanillasource.eliot.eliotc.eval.fact.Types.stringFQN
    makeORV(
      stringFQN,
      None,
      sig = OperatorResolvedExpression.ValueReference(src(typeFQN)),
      opaque = true
    )
  }

  private val defaultFacts: Seq[CompilerFact] = Seq(makeBigIntORV(), makeStringORV())

  private def runEval(expr: OperatorResolvedExpression): IO[Sem] =
    runEvalInEnv(Env.empty, expr)

  private def runEvalInEnv(env: Env, expr: OperatorResolvedExpression): IO[Sem] =
    runEvalWithFacts(env, expr, defaultFacts)

  private def runEvalWithFacts(
      expr: OperatorResolvedExpression,
      facts: Seq[CompilerFact]
  ): IO[Sem] = runEvalWithFacts(Env.empty, expr, facts)

  private def runEvalWithFacts(
      env: Env,
      expr: OperatorResolvedExpression,
      facts: Seq[CompilerFact]
  ): IO[Sem] =
    for {
      generator <- createGenerator(facts)
      result    <- Evaluator2.eval(env, expr).runA(MetaState()).run(generator).run(Chain.empty).value
    } yield result match {
      case Right((_, value)) => value
      case Left(errors)      =>
        throw new Exception(s"Evaluation failed: ${errors.map(_.message).toList.mkString(", ")}")
    }
}
