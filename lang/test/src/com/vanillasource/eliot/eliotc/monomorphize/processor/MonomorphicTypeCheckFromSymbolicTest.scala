package com.vanillasource.eliot.eliotc.monomorphize.processor

import cats.effect.IO
import com.vanillasource.eliot.eliotc.ProcessorTest
import com.vanillasource.eliot.eliotc.ast.processor.ASTParser
import com.vanillasource.eliot.eliotc.core.fact.{QualifiedName, Qualifier}
import com.vanillasource.eliot.eliotc.core.processor.CoreProcessor
import com.vanillasource.eliot.eliotc.eval.fact.{Types, Value}
import com.vanillasource.eliot.eliotc.eval.processor.{DataTypeEvaluator, ExistingNamedValueEvaluator, SystemValueEvaluator}
import com.vanillasource.eliot.eliotc.implementation.processor.{AbilityImplementationCheckProcessor, AbilityImplementationProcessor}
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, ValueFQN}
import com.vanillasource.eliot.eliotc.module.processor.*
import com.vanillasource.eliot.eliotc.monomorphize.fact.MonomorphicValue
import com.vanillasource.eliot.eliotc.matchdesugar.processor.MatchDesugaringProcessor
import com.vanillasource.eliot.eliotc.operator.processor.OperatorResolverProcessor
import com.vanillasource.eliot.eliotc.resolve.processor.ValueResolver
import com.vanillasource.eliot.eliotc.symbolic.processor.SymbolicTypeCheckProcessor
import com.vanillasource.eliot.eliotc.abilitycheck.AbilityCheckProcessor
import com.vanillasource.eliot.eliotc.token.Tokenizer

/** Tests migrated from SymbolicTypeCheckProcessorTest. These verify type checking at the monomorphize level with
  * concrete types, instead of at the symbolic level with generic types.
  */
class MonomorphicTypeCheckFromSymbolicTest
    extends ProcessorTest(
      Tokenizer(),
      ASTParser(),
      CoreProcessor(),
      SystemValueEvaluator(),
      ExistingNamedValueEvaluator(),
      DataTypeEvaluator(),
      ModuleNamesProcessor(),
      UnifiedModuleNamesProcessor(),
      ModuleValueProcessor(
        Seq(
          ModuleName.systemFunctionModuleName,
          ModuleName(ModuleName.defaultSystemPackage, "Number"),
          ModuleName(ModuleName.defaultSystemPackage, "String"),
          ModuleName(ModuleName.defaultSystemPackage, "BigInteger"),
          ModuleName(ModuleName.defaultSystemPackage, "Unit")
        )
      ),
      UnifiedModuleValueProcessor(),
      ValueResolver(),
      MatchDesugaringProcessor(),
      OperatorResolverProcessor(),
      SymbolicTypeCheckProcessor(),
      AbilityImplementationProcessor(),
      AbilityImplementationCheckProcessor(),
      AbilityCheckProcessor(),
      MonomorphicTypeCheckProcessor()
    ) {

  override val systemImports: Seq[SystemImport] = Seq(
    SystemImport("Function", "opaque type Function[A, B]\ndef apply[A, B](f: Function[A, B], a: A): B"),
    SystemImport("Type", "opaque type Type"),
    SystemImport("Number", "opaque type Int"),
    SystemImport("String", "opaque type String"),
    SystemImport("BigInteger", "opaque type BigInteger"),
    SystemImport("Unit", "opaque type Unit")
  )

  private val intType: Value =
    Types.dataType(ValueFQN(ModuleName(ModuleName.defaultSystemPackage, "Number"), QualifiedName("Int", Qualifier.Type)))
  private val stringType: Value =
    Types.dataType(ValueFQN(ModuleName(ModuleName.defaultSystemPackage, "String"), QualifiedName("String", Qualifier.Type)))

  // --- Function call tests ---

  "function call" should "compile if same number of arguments" in {
    runForErrors("data A\ndef f: A = b\ndef b: A")
      .asserting(_ shouldBe Seq.empty)
  }

  it should "not compile if call site has arguments, but definition doesn't" in {
    runForErrors("data A\ndef f: A = b(1)\ndef b: A")
      .asserting(_ should not be empty)
  }

  // --- Generic type tests ---

  "generic types" should "type check when returning itself from a parameter" in {
    runForErrors("def f[A](a: A): A = a", typeArgs = Seq(intType))
      .asserting(_ shouldBe Seq.empty)
  }

  it should "forward unification to concrete types" in {
    runForErrors("def id[A](a: A): A = a\ndef f(s: String): String = id(s)")
      .asserting(_ shouldBe Seq.empty)
  }

  it should "forward unification to concrete types in recursive setup" in {
    runForErrors("def id[A](a: A): A = a\ndef f(s: String): String = id(id(id(s)))")
      .asserting(_ shouldBe Seq.empty)
  }

  it should "fail if forward unification to concrete types produces conflict" in {
    runForErrors("def id[A](a: A): A = a\ndef f(i: BigInteger, s: String): String = id(i)")
      .asserting(_ should not be empty)
  }

  // --- Functions without body ---

  "functions without body" should "be monomorphized with simple return type" in {
    runForErrors("data A\ndef f: A")
      .asserting(_ shouldBe Seq.empty)
  }

  it should "be monomorphized with one parameter" in {
    runForErrors("data A\ndata B\ndef f(a: A): B")
      .asserting(_ shouldBe Seq.empty)
  }

  it should "be monomorphized with generic parameters" in {
    runForErrors("def f[A](a: A): A", typeArgs = Seq(intType))
      .asserting(_ shouldBe Seq.empty)
  }

  // --- Parameter usage ---

  "parameter usage" should "type check when parameter type matches return type" in {
    runForErrors("data TypeA(fieldA: TypeA)\ndef f(x: TypeA): TypeA = x")
      .asserting(_ shouldBe Seq.empty)
  }

  it should "fail when parameter type does not match return type" in {
    runForErrors("data TypeA(fieldA: TypeA)\ndata TypeB\ndef f(x: TypeA): TypeB = x")
      .asserting(_ should not be empty)
  }

  // --- Top level functions ---

  "top level functions" should "be assignable to function types" in {
    runForErrors("data Foo\ndef g(a: Foo): Foo\ndef f: Function[Foo, Foo] = g")
      .asserting(_ shouldBe Seq.empty)
  }

  // --- Apply ---

  "apply" should "type check and return B" in {
    runForErrors("def f[A, B](g: Function[A, B], a: A): B = g(a)", typeArgs = Seq(intType, stringType))
      .asserting(_ shouldBe Seq.empty)
  }

  // --- Lambda type inference ---

  "lambda type inference" should "infer parameter type for unannotated lambda from context" in {
    runForErrors("data Foo(l: Function[Unit, String])\ndef g: String\ndef f: Foo = Foo(unit -> g)")
      .asserting(_ shouldBe Seq.empty)
  }

  // --- Integer and string literals ---

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
    runForErrors("def constVal: Int\ndef f: Int = constVal")
      .asserting(_ shouldBe Seq.empty)
  }

  // --- Function application ---

  "function application" should "monomorphize generic function call" in {
    runForErrors("def id[A](a: A): A = a\ndef f: BigInteger = id(42)")
      .asserting(_ shouldBe Seq.empty)
  }

  // --- Recursion ---

  "recursion" should "handle direct recursion without infinite loop" in {
    import scala.concurrent.duration.*
    runForErrors("def f: Function[Int, Int] = f")
      .timeout(1.seconds)
      .asserting(_ shouldBe Seq.empty)
  }

  it should "handle mutual recursion without infinite loop" in {
    import scala.concurrent.duration.*
    runForErrors("def f: Function[Int, Int] = g\ndef g: Function[Int, Int] = f")
      .timeout(1.seconds)
      .asserting(_ shouldBe Seq.empty)
  }

  private def runForErrors(
      source: String,
      name: String = "f",
      typeArgs: Seq[Value] = Seq.empty
  ): IO[Seq[TestError]] =
    runGenerator(
      source,
      MonomorphicValue.Key(ValueFQN(testModuleName, default(name)), typeArgs),
      systemImports
    ).map(result => toTestErrors(result._1))
}
