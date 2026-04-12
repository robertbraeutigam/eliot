package com.vanillasource.eliot.eliotc.monomorphize3.processor

import cats.effect.IO
import com.vanillasource.eliot.eliotc.ProcessorTest
import com.vanillasource.eliot.eliotc.ast.processor.ASTParser
import com.vanillasource.eliot.eliotc.core.fact.{QualifiedName, Qualifier}
import com.vanillasource.eliot.eliotc.core.processor.CoreProcessor
import com.vanillasource.eliot.eliotc.eval.fact.Types
import com.vanillasource.eliot.eliotc.eval.processor.{
  DataTypeEvaluator,
  ExistingNamedValueEvaluator,
  SystemValueEvaluator
}
import com.vanillasource.eliot.eliotc.implementation.processor.{
  AbilityImplementationCheckProcessor,
  AbilityImplementationProcessor
}
import com.vanillasource.eliot.eliotc.matchdesugar.processor.MatchDesugaringProcessor
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, ValueFQN}
import com.vanillasource.eliot.eliotc.module.processor.*
import com.vanillasource.eliot.eliotc.monomorphize3.fact.Monomorphic3Value
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedExpression
import com.vanillasource.eliot.eliotc.operator.processor.OperatorResolverProcessor
import com.vanillasource.eliot.eliotc.pos.PositionRange
import com.vanillasource.eliot.eliotc.resolve.processor.ValueResolver
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.token.Tokenizer

class Monomorphic3TypeCheckTest
    extends ProcessorTest(
      Tokenizer(),
      ASTParser(),
      CoreProcessor(),
      SystemValueEvaluator(),
      ExistingNamedValueEvaluator(),
      DataTypeEvaluator(),
      ModuleNamesProcessor(),
      UnifiedModuleNamesProcessor(),
      ModuleValueProcessor(),
      UnifiedModuleValueProcessor(),
      ValueResolver(),
      MatchDesugaringProcessor(),
      OperatorResolverProcessor(),
      AbilityImplementationProcessor(),
      AbilityImplementationCheckProcessor(),
      SystemNativesProcessor(),
      DataTypeNativesProcessor(),
      UserValueNativesProcessor(),
      Monomorphic3Processor()
    ) {

  // --- Function call tests ---

  "function call" should "compile if same number of arguments" in {
    runForErrors("def f: String = b\ndef b: String")
      .asserting(_ shouldBe Seq.empty)
  }

  it should "not compile if call site has arguments, but definition doesn't" in {
    runForErrors("def f: String = b(1)\ndef b: String")
      .asserting(_ shouldBe Seq("Type mismatch." at "b(1)"))
  }

  it should "not compile if call site has no arguments, but definition has one" in {
    runForErrors("data A\ndef f: A = b\ndef b(x: A): A")
      .asserting(_ shouldBe Seq("Type mismatch." at "b"))
  }

  // --- Functions without body (non-generic) ---

  "functions without body" should "be monomorphized with simple return type" in {
    runForErrors("data A\ndef f: A")
      .asserting(_ shouldBe Seq.empty)
  }

  it should "be monomorphized with one parameter" in {
    runForErrors("data A\ndata B\ndef f(a: A): B")
      .asserting(_ shouldBe Seq.empty)
  }

  it should "be monomorphized with multiple parameters" in {
    runForErrors("data A\ndata B\ndata C\ndef f(a: A, b: B): C")
      .asserting(_ shouldBe Seq.empty)
  }

  // --- Parameter usage ---

  "parameter usage" should "type check when parameter type matches return type" in {
    runForErrors("data TypeA(fieldA: TypeA)\ndef f(x: TypeA): TypeA = x")
      .asserting(_ shouldBe Seq.empty)
  }

  it should "fail when parameter type does not match return type" in {
    runForErrors("data TypeA(fieldA: TypeA)\ndata TypeB\ndef f(x: TypeA): TypeB = x")
      .asserting(_ shouldBe Seq("Type mismatch." at "x"))
  }

  // --- Top level functions ---

  "top level functions" should "be assignable to function types" in {
    runForErrors("data Foo\ndef g(a: Foo): Foo\ndef f: Function[Foo, Foo] = g")
      .asserting(_ shouldBe Seq.empty)
  }

  // --- Literals ---

  "literals" should "monomorphize integer literal in body" in {
    runForErrors("def f: BigInteger = 42")
      .asserting(_ shouldBe Seq.empty)
  }

  it should "monomorphize string literal in body" in {
    runForErrors("def f: String = \"hello\"")
      .asserting(_ shouldBe Seq.empty)
  }

  // --- Value references ---

  "value references" should "monomorphize reference to non-generic value" in {
    runForErrors("def constVal: BigInteger\ndef f: BigInteger = constVal")
      .asserting(_ shouldBe Seq.empty)
  }

  // --- Error reporting ---

  "error reporting" should "issue error when referencing an undefined function" in {
    runForErrors("data A\ndef f: A = c")
      .asserting(_ shouldBe Seq("Name not defined." at "c"))
  }

  it should "fail only once when a function is used wrong" in {
    runForErrors("data A\ndata B\ndef a: A\ndef f: B = a")
      .asserting(_ shouldBe Seq("Type mismatch." at "a"))
  }

  it should "fail if parameter is of wrong type" in {
    runForErrors("data A\ndata B\ndef f(b: B): A = b")
      .asserting(_ shouldBe Seq("Type mismatch." at "b"))
  }

  private def dummySourced[T](v: T) = Sourced[T](file, PositionRange.zero, v)

  private val intType: Sourced[OperatorResolvedExpression] =
    dummySourced(OperatorResolvedExpression.ValueReference(dummySourced(Types.bigIntFQN)))

  private val stringType: Sourced[OperatorResolvedExpression] =
    dummySourced(OperatorResolvedExpression.ValueReference(dummySourced(Types.stringFQN)))

  private def runForErrors(
      source: String,
      name: String = "f",
      typeArgs: Seq[Sourced[OperatorResolvedExpression]] = Seq.empty
  ): IO[Seq[TestError]] =
    runGenerator(
      source,
      Monomorphic3Value.Key(ValueFQN(testModuleName, default(name)), typeArgs),
      systemImports
    ).map(result => toTestErrors(result._1))
}
