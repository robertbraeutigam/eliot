package com.vanillasource.eliot.eliotc.eval.util

import cats.data.Chain
import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.compiler.FactGenerator
import com.vanillasource.eliot.eliotc.core.fact.ExpressionStack
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue.*
import com.vanillasource.eliot.eliotc.eval.fact.{NamedEvaluable, Value}
import com.vanillasource.eliot.eliotc.feedback.CompilerError
import com.vanillasource.eliot.eliotc.module2.fact.{ModuleName, ValueFQN}
import com.vanillasource.eliot.eliotc.pos.PositionRange
import com.vanillasource.eliot.eliotc.processor.common.SequentialCompilerProcessors
import com.vanillasource.eliot.eliotc.processor.{CompilationProcess, CompilerFact, CompilerFactKey, CompilerProcessor}
import com.vanillasource.eliot.eliotc.resolve2.fact.Expression
import com.vanillasource.eliot.eliotc.source.content.{SourceContent, Sourced}
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

import java.io.File

class EvaluatorTest extends AsyncFlatSpec with AsyncIOSpec with Matchers {
  private val testFile       = new File("Test.els")
  private val testModuleName = ModuleName(Seq.empty, "Test")
  private val sourceContent  = SourceContent(testFile, Sourced(testFile, PositionRange.zero, "test source"))

  "evaluator" should "evaluate integer literal to ConcreteValue" in {
    runEvaluator(intLit(42)).asserting(_ shouldBe ConcreteValue(Value.LiteralInteger(42), Value.Structure(Map.empty)))
  }

  it should "evaluate string literal to ConcreteValue" in {
    runEvaluator(strLit("hello")).asserting(_ shouldBe ConcreteValue(Value.LiteralString("hello"), Value.Structure(Map.empty)))
  }

  it should "evaluate negative integer literal" in {
    runEvaluator(intLit(-123)).asserting(_ shouldBe ConcreteValue(Value.LiteralInteger(-123), Value.Structure(Map.empty)))
  }

  it should "evaluate empty string literal" in {
    runEvaluator(strLit("")).asserting(_ shouldBe ConcreteValue(Value.LiteralString(""), Value.Structure(Map.empty)))
  }

  it should "evaluate function literal with parameter reference in body" in {
    val expr = funLit("x", intLit(1), paramRef("x"))
    runEvaluator(expr).asserting {
      case FunctionLiteral("x", _, ParameterReference("x")) => succeed
      case other                                            => fail(s"Unexpected result: $other")
    }
  }

  it should "evaluate nested function literal" in {
    val expr = funLit("x", intLit(1), funLit("y", intLit(2), paramRef("x")))
    runEvaluator(expr).asserting {
      case FunctionLiteral("x", _, FunctionLiteral("y", _, ParameterReference("x"))) => succeed
      case other => fail(s"Unexpected result: $other")
    }
  }

  it should "apply function literal to argument" in {
    val fn   = funLit("x", intLit(1), paramRef("x"))
    val expr = funApp(fn, intLit(42))
    runEvaluator(expr).asserting(_ shouldBe ConcreteValue(Value.LiteralInteger(42), Value.Structure(Map.empty)))
  }

  it should "apply nested function to multiple arguments" in {
    val fn   = funLit("x", intLit(1), funLit("y", intLit(2), paramRef("x")))
    val expr = funApp(funApp(fn, intLit(1)), intLit(2))
    runEvaluator(expr).asserting(_ shouldBe ConcreteValue(Value.LiteralInteger(1), Value.Structure(Map.empty)))
  }

  it should "return inner parameter when outer is applied" in {
    val fn   = funLit("x", intLit(1), funLit("y", intLit(2), paramRef("y")))
    val expr = funApp(funApp(fn, intLit(1)), intLit(2))
    runEvaluator(expr).asserting(_ shouldBe ConcreteValue(Value.LiteralInteger(2), Value.Structure(Map.empty)))
  }

  it should "handle partial application" in {
    val fn   = funLit("x", intLit(1), funLit("y", intLit(2), paramRef("x")))
    val expr = funApp(fn, intLit(42))
    runEvaluator(expr).asserting {
      case FunctionLiteral("y", _, ConcreteValue(Value.LiteralInteger(42), _)) => succeed
      case other => fail(s"Unexpected result: $other")
    }
  }

  it should "handle shadowed parameters" in {
    val fn   = funLit("x", intLit(1), funLit("x", intLit(2), paramRef("x")))
    val expr = funApp(funApp(fn, intLit(1)), intLit(2))
    runEvaluator(expr).asserting(_ shouldBe ConcreteValue(Value.LiteralInteger(2), Value.Structure(Map.empty)))
  }

  it should "not substitute in shadowed inner function" in {
    val fn   = funLit("x", intLit(1), funLit("x", intLit(2), paramRef("x")))
    val expr = funApp(fn, intLit(42))
    runEvaluator(expr).asserting {
      case FunctionLiteral("x", _, ParameterReference("x")) => succeed
      case other => fail(s"Unexpected result: $other")
    }
  }

  it should "resolve value reference from registered fact" in {
    val vfqn = ValueFQN(testModuleName, "testValue")
    val fact = NamedEvaluable(vfqn, ConcreteValue(Value.LiteralInteger(100), Value.Structure(Map.empty)))
    runEvaluatorWithFacts(valueRef(vfqn), Seq(fact)).asserting(_ shouldBe ConcreteValue(Value.LiteralInteger(100), Value.Structure(Map.empty)))
  }

  it should "resolve function value reference and apply" in {
    val vfqn = ValueFQN(testModuleName, "identity")
    val identityFn = FunctionLiteral("x", Value.Structure(Map.empty), ParameterReference("x"))
    val fact = NamedEvaluable(vfqn, identityFn)
    val expr = funApp(valueRef(vfqn), intLit(42))
    runEvaluatorWithFacts(expr, Seq(fact)).asserting(_ shouldBe ConcreteValue(Value.LiteralInteger(42), Value.Structure(Map.empty)))
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
    val fnFact  = NamedEvaluable(fnVfqn, FunctionLiteral("x", Value.Structure(Map.empty), ParameterReference("x")))
    val expr    = funApp(valueRef(fnVfqn), valueRef(vfqn))
    runEvaluatorWithFactsAndTracking(expr, Seq(fnFact), Set(vfqn)).asserting(_ shouldBe Left("Recursive evaluation detected."))
  }

  it should "fail when applying concrete value as function" in {
    val expr = funApp(intLit(42), intLit(1))
    runEvaluatorForError(expr).asserting(_ shouldBe "Cannot apply concrete value as a function.")
  }

  it should "apply native function to concrete argument" in {
    val vfqn = ValueFQN(testModuleName, "double")
    val nativeFn = NativeFunction("x", Value.Structure(Map.empty), {
      case Value.LiteralInteger(n) => ConcreteValue(Value.LiteralInteger(n * 2), Value.Structure(Map.empty))
      case v                       => ConcreteValue(v, Value.Structure(Map.empty))
    })
    val fact = NamedEvaluable(vfqn, nativeFn)
    val expr = funApp(valueRef(vfqn), intLit(21))
    runEvaluatorWithFacts(expr, Seq(fact)).asserting(_ shouldBe ConcreteValue(Value.LiteralInteger(42), Value.Structure(Map.empty)))
  }

  it should "fail when native function receives non-concrete argument" in {
    val vfqn = ValueFQN(testModuleName, "nativeFn")
    val nativeFn = NativeFunction("x", Value.Structure(Map.empty), v => ConcreteValue(v, Value.Structure(Map.empty)))
    val fact = NamedEvaluable(vfqn, nativeFn)
    val outerFn = funLit("y", intLit(1), funApp(valueRef(vfqn), paramRef("y")))
    runEvaluatorWithFactsForError(outerFn, Seq(fact)).asserting(_ shouldBe "Native function requires concrete argument.")
  }

  it should "leave function application unreduced when target is parameter reference" in {
    val fn   = funLit("f", intLit(1), funApp(paramRef("f"), intLit(42)))
    runEvaluator(fn).asserting {
      case FunctionLiteral("f", _, FunctionApplication(ParameterReference("f"), ConcreteValue(Value.LiteralInteger(42), _))) =>
        succeed
      case other => fail(s"Unexpected result: $other")
    }
  }

  it should "reduce nested function applications" in {
    val inner = funLit("x", intLit(1), paramRef("x"))
    val outer = funLit("y", intLit(1), funApp(inner, paramRef("y")))
    val expr  = funApp(outer, intLit(42))
    runEvaluator(expr).asserting(_ shouldBe ConcreteValue(Value.LiteralInteger(42), Value.Structure(Map.empty)))
  }

  it should "reduce function application inside function body" in {
    val addOne = funLit("x", intLit(1), intLit(1))
    val fn     = funLit("y", intLit(1), funApp(addOne, paramRef("y")))
    runEvaluator(fn).asserting {
      case FunctionLiteral("y", _, ConcreteValue(Value.LiteralInteger(1), _)) => succeed
      case other => fail(s"Unexpected result: $other")
    }
  }

  it should "evaluate complex nested expression" in {
    val const = funLit("x", intLit(1), funLit("y", intLit(1), paramRef("x")))
    val expr  = funApp(funApp(const, intLit(1)), intLit(2))
    runEvaluator(expr).asserting(_ shouldBe ConcreteValue(Value.LiteralInteger(1), Value.Structure(Map.empty)))
  }

  it should "allow different values with same name in tracking set" in {
    val vfqn1 = ValueFQN(testModuleName, "a")
    val vfqn2 = ValueFQN(ModuleName(Seq("other"), "Module"), "a")
    val fact1 = NamedEvaluable(vfqn1, ConcreteValue(Value.LiteralInteger(1), Value.Structure(Map.empty)))
    val fact2 = NamedEvaluable(vfqn2, ConcreteValue(Value.LiteralInteger(2), Value.Structure(Map.empty)))
    val expr  = valueRef(vfqn2)
    runEvaluatorWithFactsAndTracking(expr, Seq(fact1, fact2), Set(vfqn1)).asserting {
      case Right(ConcreteValue(Value.LiteralInteger(2), _)) => succeed
      case other => fail(s"Unexpected result: $other")
    }
  }

  it should "abort when top-level parameter reference is unbound" in {
    val expr = paramRef("unbound")
    runEvaluatorAborts(expr).asserting(_ shouldBe true)
  }

  it should "abort when top-level function application cannot be reduced" in {
    val fn   = funLit("x", intLit(1), funApp(paramRef("f"), paramRef("x")))
    val expr = funApp(fn, intLit(1))
    runEvaluatorAborts(expr).asserting(_ shouldBe true)
  }

  it should "chain multiple value references" in {
    val vfqnA = ValueFQN(testModuleName, "a")
    val vfqnB = ValueFQN(testModuleName, "b")
    val vfqnC = ValueFQN(testModuleName, "c")
    val factC = NamedEvaluable(vfqnC, ConcreteValue(Value.LiteralInteger(42), Value.Structure(Map.empty)))
    val factB = NamedEvaluable(vfqnB, factC.value)
    val factA = NamedEvaluable(vfqnA, factB.value)
    runEvaluatorWithFacts(valueRef(vfqnA), Seq(factA, factB, factC))
      .asserting(_ shouldBe ConcreteValue(Value.LiteralInteger(42), Value.Structure(Map.empty)))
  }

  it should "evaluate deeply nested function applications" in {
    val id   = funLit("x", intLit(1), paramRef("x"))
    val expr = funApp(funApp(funApp(funApp(id, id), id), id), intLit(42))
    runEvaluator(expr).asserting(_ shouldBe ConcreteValue(Value.LiteralInteger(42), Value.Structure(Map.empty)))
  }

  it should "correctly substitute in complex body" in {
    val fn   = funLit("x", intLit(1), funApp(funLit("y", intLit(1), paramRef("x")), paramRef("x")))
    val expr = funApp(fn, intLit(99))
    runEvaluator(expr).asserting(_ shouldBe ConcreteValue(Value.LiteralInteger(99), Value.Structure(Map.empty)))
  }

  it should "detect recursion in nested function literal body" in {
    val vfqn = ValueFQN(testModuleName, "rec")
    val expr = funLit("x", intLit(1), valueRef(vfqn))
    runEvaluatorWithTracking(expr, Set(vfqn)).asserting(_ shouldBe Left("Recursive evaluation detected."))
  }

  it should "handle function literal with no runtime body" in {
    val expr = Expression.FunctionLiteral(
      sourced("x"),
      sourced(ExpressionStack(Seq(intLit(1)), true)),
      sourced(ExpressionStack(Seq.empty, false))
    )
    runEvaluatorAborts(expr).asserting(_ shouldBe true)
  }

  it should "handle function application with no runtime target" in {
    val expr = Expression.FunctionApplication(
      sourced(ExpressionStack(Seq.empty, false)),
      sourced(ExpressionStack(Seq(intLit(1)), true))
    )
    runEvaluatorAborts(expr).asserting(_ shouldBe true)
  }

  it should "handle function application with no runtime argument" in {
    val fn   = funLit("x", intLit(1), paramRef("x"))
    val expr = Expression.FunctionApplication(
      sourced(ExpressionStack(Seq(fn), true)),
      sourced(ExpressionStack(Seq.empty, false))
    )
    runEvaluatorAborts(expr).asserting(_ shouldBe true)
  }

  private def sourced[T](value: T): Sourced[T] = Sourced(testFile, PositionRange.zero, value)

  private def intLit(value: BigInt): Expression = Expression.IntegerLiteral(sourced(value))

  private def strLit(value: String): Expression = Expression.StringLiteral(sourced(value))

  private def paramRef(name: String): Expression = Expression.ParameterReference(sourced(name))

  private def valueRef(vfqn: ValueFQN): Expression = Expression.ValueReference(sourced(vfqn))

  private def funLit(param: String, paramTypeExpr: Expression, body: Expression): Expression =
    Expression.FunctionLiteral(
      sourced(param),
      sourced(ExpressionStack(Seq(paramTypeExpr), true)),
      sourced(ExpressionStack(Seq(body), true))
    )

  private def funApp(target: Expression, arg: Expression): Expression =
    Expression.FunctionApplication(
      sourced(ExpressionStack(Seq(target), true)),
      sourced(ExpressionStack(Seq(arg), true))
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
      _         <- facts.traverse_(generator.registerFact)
      result    <- Evaluator.evaluate(expression).run(generator).run(Chain.empty).value
    } yield result match {
      case Right((_, value)) => value
      case Left(errors)      => throw new Exception(s"Evaluation failed: ${errors.map(_.message).toList.mkString(", ")}")
    }

  private def runEvaluatorAborts(expression: Expression): IO[Boolean] =
    for {
      generator <- FactGenerator.create(SequentialCompilerProcessors(Seq.empty))
      _         <- generator.registerFact(sourceContent)
      result    <- Evaluator.evaluate(expression).run(generator).run(Chain.empty).value
    } yield result match {
      case Left(_) => true
      case _       => false
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
      _         <- facts.traverse_(generator.registerFact)
      result    <- Evaluator.evaluate(expression).run(generator).run(Chain.empty).value
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
      _         <- facts.traverse_(generator.registerFact)
      result    <- Evaluator.evaluate(expression, evaluating).run(generator).run(Chain.empty).value
    } yield result match {
      case Right((_, value))                     => Right(value)
      case Left(errors) if errors.nonEmpty       => Left(errors.toList.head.message)
      case Left(_)                               => Left("Unknown error")
    }
}
