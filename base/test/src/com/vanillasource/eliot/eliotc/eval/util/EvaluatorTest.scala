package com.vanillasource.eliot.eliotc.eval.util

import cats.data.{Chain, NonEmptySeq}
import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.compiler.FactGenerator
import com.vanillasource.eliot.eliotc.core.fact.TypeStack
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue.*
import com.vanillasource.eliot.eliotc.eval.fact.{NamedEvaluable, Value}
import com.vanillasource.eliot.eliotc.eval.fact.Value.Type
import com.vanillasource.eliot.eliotc.eval.fact.Types.{bigIntType, stringType}
import com.vanillasource.eliot.eliotc.feedback.CompilerError
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, ValueFQN}
import com.vanillasource.eliot.eliotc.pos.PositionRange
import com.vanillasource.eliot.eliotc.processor.common.SequentialCompilerProcessors
import com.vanillasource.eliot.eliotc.processor.{CompilationProcess, CompilerFact, CompilerFactKey, CompilerProcessor}
import com.vanillasource.eliot.eliotc.resolve.fact.Expression
import com.vanillasource.eliot.eliotc.source.content.{SourceContent, Sourced}
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

import java.net.URI

class EvaluatorTest extends AsyncFlatSpec with AsyncIOSpec with Matchers {
  private val testFile       = URI.create("Test.els")
  private val testModuleName = ModuleName(Seq.empty, "Test")
  private val sourceContent  = SourceContent(testFile, Sourced(testFile, PositionRange.zero, "test source"))

  // Type facts for use in function parameter types
  private val bigIntTypeVfqn = ValueFQN(testModuleName, "BigIntType")
  private val bigIntTypeFact = NamedEvaluable(bigIntTypeVfqn, ConcreteValue(bigIntType))
  private val stringTypeVfqn = ValueFQN(testModuleName, "StringType")
  private val stringTypeFact = NamedEvaluable(stringTypeVfqn, ConcreteValue(stringType))

  "evaluator" should "evaluate integer literal to ConcreteValue" in {
    runEvaluator(intLit(42)).asserting(_ shouldBe ConcreteValue(Value.Direct(42, bigIntType)))
  }

  it should "evaluate string literal to ConcreteValue" in {
    runEvaluator(strLit("hello")).asserting(_ shouldBe ConcreteValue(Value.Direct("hello", stringType)))
  }

  it should "evaluate negative integer literal" in {
    runEvaluator(intLit(-123)).asserting(_ shouldBe ConcreteValue(Value.Direct(-123, bigIntType)))
  }

  it should "evaluate empty string literal" in {
    runEvaluator(strLit("")).asserting(_ shouldBe ConcreteValue(Value.Direct("", stringType)))
  }

  it should "evaluate function literal with parameter reference in body" in {
    val expr = intFunLit("x", paramRef("x"))
    runEvaluator(expr).asserting {
      case FunctionLiteral("x", _, ParameterReference("x", _)) => succeed
      case other                                            => fail(s"Unexpected result: $other")
    }
  }

  it should "evaluate nested function literal" in {
    val expr = intFunLit("x", intFunLit("y", paramRef("x")))
    runEvaluator(expr).asserting {
      case FunctionLiteral("x", _, FunctionLiteral("y", _, ParameterReference("x", _))) => succeed
      case other => fail(s"Unexpected result: $other")
    }
  }

  it should "apply function literal to argument" in {
    val fn   = intFunLit("x", paramRef("x"))
    val expr = funApp(fn, intLit(42))
    runEvaluator(expr).asserting(_ shouldBe ConcreteValue(Value.Direct(42, bigIntType)))
  }

  it should "apply nested function to multiple arguments" in {
    val fn   = intFunLit("x", intFunLit("y", paramRef("x")))
    val expr = funApp(funApp(fn, intLit(1)), intLit(2))
    runEvaluator(expr).asserting(_ shouldBe ConcreteValue(Value.Direct(1, bigIntType)))
  }

  it should "return inner parameter when outer is applied" in {
    val fn   = intFunLit("x", intFunLit("y", paramRef("y")))
    val expr = funApp(funApp(fn, intLit(1)), intLit(2))
    runEvaluator(expr).asserting(_ shouldBe ConcreteValue(Value.Direct(2, bigIntType)))
  }

  it should "handle partial application" in {
    val fn   = intFunLit("x", intFunLit("y", paramRef("x")))
    val expr = funApp(fn, intLit(42))
    runEvaluator(expr).asserting {
      case FunctionLiteral("y", _, ConcreteValue(Value.Direct(42, _))) => succeed
      case other => fail(s"Unexpected result: $other")
    }
  }

  it should "handle shadowed parameters" in {
    val fn   = intFunLit("x", intFunLit("x", paramRef("x")))
    val expr = funApp(funApp(fn, intLit(1)), intLit(2))
    runEvaluator(expr).asserting(_ shouldBe ConcreteValue(Value.Direct(2, bigIntType)))
  }

  it should "not substitute in shadowed inner function" in {
    val fn   = intFunLit("x", intFunLit("x", paramRef("x")))
    val expr = funApp(fn, intLit(42))
    runEvaluator(expr).asserting {
      case FunctionLiteral("x", _, ParameterReference("x", _)) => succeed
      case other => fail(s"Unexpected result: $other")
    }
  }

  it should "resolve value reference from registered fact" in {
    val vfqn = ValueFQN(testModuleName, "testValue")
    val fact = NamedEvaluable(vfqn, ConcreteValue(Value.Direct(100, bigIntType)))
    runEvaluatorWithFacts(valueRef(vfqn), Seq(fact)).asserting(_ shouldBe ConcreteValue(Value.Direct(100, bigIntType)))
  }

  it should "resolve function value reference and apply" in {
    val vfqn = ValueFQN(testModuleName, "identity")
    val identityFn = FunctionLiteral("x", bigIntType, ParameterReference("x", bigIntType))
    val fact = NamedEvaluable(vfqn, identityFn)
    val expr = funApp(valueRef(vfqn), intLit(42))
    runEvaluatorWithFacts(expr, Seq(fact)).asserting(_ shouldBe ConcreteValue(Value.Direct(42, bigIntType)))
  }

  it should "detect direct recursion" in {
    val vfqn = ValueFQN(testModuleName, "recursive")
    val expr = valueRef(vfqn)
    runEvaluatorWithTracking(expr, Set(vfqn)).asserting(_ shouldBe Left("Recursive evaluation detected."))
  }

  it should "detect recursion through function application target" in {
    val vfqn = ValueFQN(testModuleName, "recursive")
    val expr = funApp(valueRef(vfqn), intLit(1))
    runEvaluatorWithTracking(expr, Set(vfqn)).asserting(_ shouldBe Left("Recursive evaluation detected."))
  }

  it should "detect recursion through function application argument" in {
    val vfqn    = ValueFQN(testModuleName, "recursive")
    val fnVfqn  = ValueFQN(testModuleName, "fn")
    val fnFact  = NamedEvaluable(fnVfqn, FunctionLiteral("x", bigIntType, ParameterReference("x", bigIntType)))
    val expr    = funApp(valueRef(fnVfqn), valueRef(vfqn))
    runEvaluatorWithFactsAndTracking(expr, Seq(fnFact), Set(vfqn)).asserting(_ shouldBe Left("Recursive evaluation detected."))
  }

  it should "fail when applying concrete value as function" in {
    val expr = funApp(intLit(42), intLit(1))
    runEvaluatorForError(expr).asserting(_ shouldBe "Could not reduce function application.")
  }

  it should "fail when argument type does not match parameter type" in {
    val fn   = intFunLit("x", paramRef("x"))
    val expr = funApp(fn, strLit("hello"))
    runEvaluatorForError(expr).asserting(_.contains("Type mismatch") shouldBe true)
  }

  it should "apply native function to concrete argument" in {
    val vfqn = ValueFQN(testModuleName, "double")
    val nativeFn = NativeFunction(bigIntType, {
      case Value.Direct(n: BigInt, t) => ConcreteValue(Value.Direct(n * 2, t))
      case v                          => ConcreteValue(v)
    })
    val fact = NamedEvaluable(vfqn, nativeFn)
    val expr = funApp(valueRef(vfqn), intLit(21))
    runEvaluatorWithFacts(expr, Seq(fact)).asserting(_ shouldBe ConcreteValue(Value.Direct(42, bigIntType)))
  }

  it should "leave native function application unreduced when argument is not concrete" in {
    val vfqn = ValueFQN(testModuleName, "nativeFn")
    val nativeFn = NativeFunction(bigIntType, v => ConcreteValue(v))
    val fact = NamedEvaluable(vfqn, nativeFn)
    val outerFn = intFunLit("y", funApp(valueRef(vfqn), paramRef("y")))
    runEvaluatorWithFacts(outerFn, Seq(fact)).asserting {
      case FunctionLiteral("y", _, FunctionApplication(NativeFunction(_, _), ParameterReference("y", _))) => succeed
      case other => fail(s"Unexpected result: $other")
    }
  }

  it should "leave function application unreduced when target is parameter reference" in {
    val fn   = intFunLit("f", funApp(paramRef("f"), intLit(42)))
    runEvaluator(fn).asserting {
      case FunctionLiteral("f", _, FunctionApplication(ParameterReference("f", _), ConcreteValue(Value.Direct(42, _)))) =>
        succeed
      case other => fail(s"Unexpected result: $other")
    }
  }

  it should "reduce nested function applications" in {
    val inner = intFunLit("x", paramRef("x"))
    val outer = intFunLit("y", funApp(inner, paramRef("y")))
    val expr  = funApp(outer, intLit(42))
    runEvaluator(expr).asserting(_ shouldBe ConcreteValue(Value.Direct(42, bigIntType)))
  }

  it should "reduce function application inside function body" in {
    val addOne = intFunLit("x", intLit(1))
    val fn     = intFunLit("y", funApp(addOne, paramRef("y")))
    runEvaluator(fn).asserting {
      case FunctionLiteral("y", _, ConcreteValue(Value.Direct(1, _))) => succeed
      case other => fail(s"Unexpected result: $other")
    }
  }

  it should "evaluate complex nested expression" in {
    val const = intFunLit("x", intFunLit("y", paramRef("x")))
    val expr  = funApp(funApp(const, intLit(1)), intLit(2))
    runEvaluator(expr).asserting(_ shouldBe ConcreteValue(Value.Direct(1, bigIntType)))
  }

  it should "allow different values with same name in tracking set" in {
    val vfqn1 = ValueFQN(testModuleName, "a")
    val vfqn2 = ValueFQN(ModuleName(Seq("other"), "Module"), "a")
    val fact1 = NamedEvaluable(vfqn1, ConcreteValue(Value.Direct(1, bigIntType)))
    val fact2 = NamedEvaluable(vfqn2, ConcreteValue(Value.Direct(2, bigIntType)))
    val expr  = valueRef(vfqn2)
    runEvaluatorWithFactsAndTracking(expr, Seq(fact1, fact2), Set(vfqn1)).asserting {
      case Right(ConcreteValue(Value.Direct(2, _))) => succeed
      case other => fail(s"Unexpected result: $other")
    }
  }

  it should "abort when top-level parameter reference is unbound" in {
    val expr = paramRef("unbound")
    runEvaluatorForError(expr).asserting(_ shouldBe "Unknown parameter: unbound")
  }

  it should "abort when top-level function application cannot be reduced" in {
    val fn   = intFunLit("x", funApp(paramRef("f"), paramRef("x")))
    val expr = funApp(fn, intLit(1))
    runEvaluatorForError(expr).asserting(_ shouldBe "Unknown parameter: f")
  }

  it should "chain multiple value references" in {
    val vfqnA = ValueFQN(testModuleName, "a")
    val vfqnB = ValueFQN(testModuleName, "b")
    val vfqnC = ValueFQN(testModuleName, "c")
    val factC = NamedEvaluable(vfqnC, ConcreteValue(Value.Direct(42, bigIntType)))
    val factB = NamedEvaluable(vfqnB, factC.value)
    val factA = NamedEvaluable(vfqnA, factB.value)
    runEvaluatorWithFacts(valueRef(vfqnA), Seq(factA, factB, factC))
      .asserting(_ shouldBe ConcreteValue(Value.Direct(42, bigIntType)))
  }

  it should "evaluate deeply nested function applications" in {
    val id   = intFunLit("x", paramRef("x"))
    val expr = funApp(funApp(funApp(funApp(id, id), id), id), intLit(42))
    runEvaluator(expr).asserting(_ shouldBe ConcreteValue(Value.Direct(42, bigIntType)))
  }

  it should "correctly substitute in complex body" in {
    val fn   = intFunLit("x", funApp(intFunLit("y", paramRef("x")), paramRef("x")))
    val expr = funApp(fn, intLit(99))
    runEvaluator(expr).asserting(_ shouldBe ConcreteValue(Value.Direct(99, bigIntType)))
  }

  it should "detect recursion in nested function literal body" in {
    val vfqn = ValueFQN(testModuleName, "rec")
    val expr = intFunLit("x", valueRef(vfqn))
    runEvaluatorWithTracking(expr, Set(vfqn)).asserting(_ shouldBe Left("Recursive evaluation detected."))
  }

  private def sourced[T](value: T): Sourced[T] = Sourced(testFile, PositionRange.zero, value)

  private def intLit(value: BigInt): Expression = Expression.IntegerLiteral(sourced(value))

  private def strLit(value: String): Expression = Expression.StringLiteral(sourced(value))

  private def paramRef(name: String): Expression = Expression.ParameterReference(sourced(name))

  private def valueRef(vfqn: ValueFQN): Expression = Expression.ValueReference(sourced(vfqn))

  private def funLit(param: String, paramTypeExpr: Expression, body: Expression): Expression =
    Expression.FunctionLiteral(
      sourced(param),
      sourced(TypeStack(NonEmptySeq.of(paramTypeExpr))),
      sourced(TypeStack(NonEmptySeq.of(body)))
    )

  /** Function literal with BigInt parameter type */
  private def intFunLit(param: String, body: Expression): Expression =
    funLit(param, valueRef(bigIntTypeVfqn), body)

  private def funApp(target: Expression, arg: Expression): Expression =
    Expression.FunctionApplication(
      sourced(TypeStack(NonEmptySeq.of(target))),
      sourced(TypeStack(NonEmptySeq.of(arg)))
    )

  private def runEvaluator(expression: Expression): IO[InitialExpressionValue] =
    runEvaluatorWithFacts(expression, Seq.empty)

  private def runEvaluatorWithFacts(
      expression: Expression,
      facts: Seq[CompilerFact]
  ): IO[InitialExpressionValue] =
    for {
      generator <- FactGenerator.create(SequentialCompilerProcessors(Seq.empty))
      _         <- generator.registerFact(sourceContent)
      _         <- generator.registerFact(bigIntTypeFact)
      _         <- generator.registerFact(stringTypeFact)
      _         <- facts.traverse_(generator.registerFact)
      result    <- Evaluator.evaluate(sourced(expression)).run(generator).run(Chain.empty).value
    } yield result match {
      case Right((_, value)) => value
      case Left(errors)      => throw new Exception(s"Evaluation failed: ${errors.map(_.message).toList.mkString(", ")}")
    }

  private def runEvaluatorForError(expression: Expression): IO[String] =
    runEvaluatorWithFactsForError(expression, Seq.empty)

  private def runEvaluatorWithFactsForError(
      expression: Expression,
      facts: Seq[CompilerFact]
  ): IO[String] =
    for {
      generator <- FactGenerator.create(SequentialCompilerProcessors(Seq.empty))
      _         <- generator.registerFact(sourceContent)
      _         <- generator.registerFact(bigIntTypeFact)
      _         <- generator.registerFact(stringTypeFact)
      _         <- facts.traverse_(generator.registerFact)
      result    <- Evaluator.evaluate(sourced(expression)).run(generator).run(Chain.empty).value
    } yield result match {
      case Right((errors, _)) if errors.nonEmpty => errors.toList.head.message
      case Left(errors) if errors.nonEmpty       => errors.toList.head.message
      case _                                     => throw new Exception("Expected error but evaluation succeeded")
    }

  private def runEvaluatorWithTracking(
      expression: Expression,
      evaluating: Set[ValueFQN]
  ): IO[Either[String, InitialExpressionValue]] =
    runEvaluatorWithFactsAndTracking(expression, Seq.empty, evaluating)

  private def runEvaluatorWithFactsAndTracking(
      expression: Expression,
      facts: Seq[CompilerFact],
      evaluating: Set[ValueFQN]
  ): IO[Either[String, InitialExpressionValue]] =
    for {
      generator <- FactGenerator.create(SequentialCompilerProcessors(Seq.empty))
      _         <- generator.registerFact(sourceContent)
      _         <- generator.registerFact(bigIntTypeFact)
      _         <- generator.registerFact(stringTypeFact)
      _         <- facts.traverse_(generator.registerFact)
      result    <- Evaluator.evaluate(sourced(expression), evaluating).run(generator).run(Chain.empty).value
    } yield result match {
      case Right((_, value))                     => Right(value)
      case Left(errors) if errors.nonEmpty       => Left(errors.toList.head.message)
      case Left(_)                               => Left("Unknown error")
    }
}
