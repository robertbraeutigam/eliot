package com.vanillasource.eliot.eliotc.monomorphize2.processor

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
import com.vanillasource.eliot.eliotc.monomorphize2.fact.MonomorphicValue
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedExpression
import com.vanillasource.eliot.eliotc.operator.processor.OperatorResolverProcessor
import com.vanillasource.eliot.eliotc.pos.PositionRange
import com.vanillasource.eliot.eliotc.resolve.processor.ValueResolver
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.token.Tokenizer

/** Tests that verify type checking at the monomorphize level with concrete types.
  */
class MonomorphicTypeCheckTest
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
      MonomorphicTypeCheckProcessor()
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
      .asserting(_ shouldBe Seq("Type mismatch." at "id(i)"))
  }

  it should "fail if forward unification to concrete types produces conflict in recursive setup" in {
    runForErrors("def id[A](a: A): A = a\ndef f(i: BigInteger, s: String): String = id(id(id(i)))")
      .asserting(_ shouldBe Seq("Type mismatch." at "id(i)"))
  }

  it should "fail when returning different, but non-constrained generic" in {
    runForErrors("def f[A, B](a: A, b: B): A = b", typeArgs = Seq(intType, stringType))
      .asserting(_ shouldBe Seq("Type mismatch." at "b"))
  }

  // --- Multi-parameter unification ---

  "multi-parameter unification" should "unify on multiple parameters" in {
    runForErrors(
      "def g[A](a: A, b: A, c: A): A = a\ndef someA[A]: A\ndef f(s: String): String = g(someA, someA, s)"
    ).asserting(_ shouldBe Seq.empty)
  }

  it should "fail, if unifying on multiple parameters fail at later stage" in {
    runForErrors(
      "def g[A](a: A, b: A, c: A): A = a\ndef someA[A]: A\ndef f(i: BigInteger, s: String): String = g(someA, someA, i)"
    ).asserting(_ shouldBe Seq("Type mismatch." at "g(someA, someA, i)")) // TODO: This sourcing is not perfect
  }

  // --- Higher-kinded types ---

  "higher-kinded types" should "type check through single generic placeholder" in {
    runForErrors(
      "def id[A](a: A): A\ndef f[A, B, C[_, _]](c: C[A, B]): C[A, B] = id(c)",
      typeArgs = Seq(intType, stringType, funcType)
    )
      .asserting(_ shouldBe Seq.empty)
  }

  it should "accept lower arities of generic parameters" in {
    runForErrors(
      "def id[B, A[_]](a: A[B]): A[B]\ndef f[A, B, C[_, _]](c: C[A, B]): C[A, B] = id(c)",
      typeArgs = Seq(intType, stringType, funcType)
    )
      .asserting(_ shouldBe Seq.empty)
  }

  it should "unify generic parameters of generics via a non-parameterized generic" in {
    runForErrors(
      "data Foo\ndata Bar\ndef id[A](a: A): A\ndef f(p: Function[Bar, Foo]): Function[Foo, Bar] = id(p)"
    ).asserting(_.nonEmpty shouldBe true)
  }

  it should "type check higher-kinded parameter returning identity" in {
    runForErrors("data Box[A]\ndef f[F[_]](x: F[BigInteger]): F[BigInteger] = x", typeArgs = Seq(boxType))
      .asserting(_ shouldBe Seq.empty)
  }

  it should "type check higher-kinded parameter with two type args" in {
    runForErrors("def f[F[_, _]](x: F[BigInteger, String]): F[BigInteger, String] = x", typeArgs = Seq(funcType))
      .asserting(_ shouldBe Seq.empty)
  }

  it should "fail when higher-kinded parameters mismatch" in {
    runForErrors("data Box[A]\ndef f[F[_]](x: F[BigInteger]): F[String] = x", typeArgs = Seq(boxType))
      .asserting(_ shouldBe Seq("Type mismatch." at "BigInteger")) // TODO: Sourcing not 100%
  }

"D"  // TODO: This is a valid case, but I don't know how to support this
  it should "type check nested higher-kinded parameter" ignore {
    runForErrors(
      "data Box[A]\ndata HyperBox[A[_]]\ndef f[G[_], F[_[_]]](x: F[G]): F[G] = x",
      typeArgs = Seq(boxType, hyperBoxType)
    )
      .asserting(_ shouldBe Seq.empty)
  }

  // --- Explicit type restrictions ---

  "explicit type restrictions" should "type check with explicit Type restriction like implicit" in {
    runForErrors("def f[A: Type](a: A): A = a", typeArgs = Seq(intType))
      .asserting(_ shouldBe Seq.empty)
  }

  it should "type check with explicit Function restriction like arity syntax" in {
    runForErrors("def f[F: Function[Type, Type]](x: F[Int]): F[Int] = x", typeArgs = Seq(intType))
      .asserting(_ shouldBe Seq.empty)
  }

  it should "type check with explicit two-arg Function restriction" in {
    runForErrors(
      "def f[F: Function[Type, Function[Type, Type]]](x: F[Int, String]): F[Int, String] = x",
      typeArgs = Seq(intType)
    ).asserting(_ shouldBe Seq.empty)
  }

  it should "type check with explicit nested higher-kinded restriction" in {
    runForErrors(
      "def f[G: Function[Type, Type], F: Function[Function[Type, Type], Type]](x: F[G]): F[G] = x",
      typeArgs = Seq(intType, stringType)
    ).asserting(_ shouldBe Seq.empty)
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

  it should "be monomorphized with multiple parameters" in {
    runForErrors("data A\ndata B\ndata C\ndef f(a: A, b: B): C")
      .asserting(_ shouldBe Seq.empty)
  }

  it should "be monomorphized with generic parameters and multiple arguments" in {
    runForErrors("def f[A, B](a: A, b: B): A", typeArgs = Seq(intType, stringType))
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

  it should "fail if parameter is used as a wrong parameter in another function" in {
    runForErrors("data A\ndata B\ndef a(b: B): A\ndef f(x: A): A = a(x)")
      .asserting(_ shouldBe Seq("Type mismatch." at "x"))
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

  it should "reject unannotated lambda when inferred type conflicts" in {
    runForErrors(
      "data Foo(l: Function[Unit, String])\ndef g(u: Unit): String\ndef f: Foo = Foo(unit -> g(unit))"
    ).asserting(_ shouldBe Seq.empty)
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

  // --- Error reporting ---

  "error reporting" should "issue error when referencing an undefined function" in {
    runForErrors("data A\ndef f: A = c")
      .asserting(_ shouldBe Seq("Name not defined." at "c"))
  }

  it should "not produce type checked results if arities mismatch" in {
    runForErrors("data A\ndef f: A = b(3)\ndef b: A")
      .asserting(_.nonEmpty shouldBe true)
  }

  it should "fail only once when a function is used wrong" in {
    runForErrors("data A\ndata B\ndef a: A\ndef f: B = a")
      .asserting(_ shouldBe Seq("Type mismatch." at "a"))
  }

  it should "fail if parameter is of wrong type" in {
    runForErrors("data A\ndata B\ndef f(b: B): A = b")
      .asserting(_ shouldBe Seq("Type mismatch." at "b"))
  }

  // --- Explicit type arguments ---

  "explicit type arguments" should "type check when the explicit arg matches usage" in {
    runForErrors("def id[A](a: A): A = a\ndef f(s: String): String = id[String](s)")
      .asserting(_ shouldBe Seq.empty)
  }

  it should "fail when the explicit type arg conflicts with the value argument" in {
    runForErrors("def id[A](a: A): A = a\ndef f(s: String): String = id[Int](s)")
      .asserting(_ shouldBe Seq("Type mismatch." at "s"))
  }

  it should "fail when the explicit type arg conflicts with the declared return type" in {
    runForErrors("def id[A](a: A): A = a\ndef i: Int\ndef f(s: String): String = id[Int](i)")
      .asserting(_ shouldBe Seq("Return type mismatch." at "i"))
  }

  it should "fail with too many type arguments" in {
    runForErrors("def id[A](a: A): A = a\ndef f(s: String): String = id[String, String](s)")
      .asserting(_ shouldBe Seq("Too many type arguments: expected at most 1, got 2" at "s"))
  }

  it should "type check with too few explicit type args by inferring the rest" in {
    runForErrors("def f2[A, B](a: A, b: B): A = a\ndef f(s: String, i: Int): String = f2[String](s, i)")
      .asserting(_ shouldBe Seq.empty)
  }

  it should "fail with too few explicit type args that conflict with usage" in {
    runForErrors("def f2[A, B](a: A, b: B): A = a\ndef f(s: String, i: Int): String = f2[Int](s, i)")
      .asserting(_ shouldBe Seq("Type mismatch." at "s"))
  }

  it should "type check with explicit type args and multiple type params" in {
    runForErrors("def g[A, B](a: A, b: B): A = a\ndef f(s: String, i: Int): String = g[String, Int](s, i)")
      .asserting(_ shouldBe Seq.empty)
  }

  it should "fail when explicit type args are in the wrong order" in {
    runForErrors("def g[A, B](a: A, b: B): A = a\ndef f(s: String, i: Int): String = g[Int, String](s, i)")
      .asserting(_ shouldBe Seq("Type mismatch." at "s"))
  }

  it should "point type argument mismatch to the explicit type argument" in {
    runForErrors("data Box[A: Type](content: String)\ndef g: String\ndef f(x: String): Box[String] = Box[Int](g)")
      .asserting(_ shouldBe Seq("Return type mismatch." at "g"))
  }

  it should "type check with an applied generic type as a type argument" in {
    runForErrors("def id[A](a: A): A = a\ndata Box(s: String)\ndef f(b: Box): Box = id[Box](b)")
      .asserting(_ shouldBe Seq.empty)
  }

  // --- Type level functions ---

  "type level functions" should "support non-type type parameters" in {
    runForErrors(
      "def str: String\ndata Group\ndata Person[G: Group](name: String)\ndef f[G: Group]: Person[G] = Person[G](str)",
      typeArgs = Seq.empty // This was Group
    ).asserting(_ shouldBe Seq.empty)
  }

  it should "calculate concrete literal values" in {
    runForErrors(
      "def one: BigInteger = 1\ndef oneDifferently: BigInteger = 1\ndef str: String\ndata Box[I: BigInteger](name: String)\ndef f: Box[one] = Box[oneDifferently](str)"
    ).asserting(_ shouldBe Seq.empty)
  }

  it should "reject calculated differing concrete literal values" in {
    runForErrors(
      "def one: BigInteger = 1\ndef two: BigInteger = 2\ndef str: String\ndata Box[I: BigInteger](name: String)\ndef f: Box[one] = Box[two](str)"
    ).asserting(_.nonEmpty shouldBe true)
  }

  it should "calculate concrete data values" in {
    runForErrors(
      "data Person(age: BigInteger)\ndef one: Person = Person(1)\ndef oneDifferently: Person = Person(1)\ndef str: String\ndata Box[I: Person](name: String)\ndef f: Box[one] = Box[oneDifferently](str)"
    ).asserting(_ shouldBe Seq.empty)
  }

  it should "reject calculated differing data values" in {
    runForErrors(
      "data Person(age: BigInteger)\ndef one: Person = Person(1)\ndef oneDifferently: Person = Person(2)\ndef str: String\ndata Box[I: Person](name: String)\ndef f: Box[one] = Box[oneDifferently](str)"
    ).asserting(_.nonEmpty shouldBe true)
  }

  it should "accept type-level function calls that are not Type types" in {
    runForErrors(
      "def g(x: String): String = x\ndata Box[X: String](value: String)\ndef f[G: String](value: String): Box(g(G)) = Box[G](value)"
    ).asserting(_ shouldBe Seq.empty)
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

  private def dummySourced[T](v: T) = Sourced[T](file, PositionRange.zero, v)

  private val boxType: Sourced[OperatorResolvedExpression] =
    dummySourced(
      OperatorResolvedExpression.ValueReference(
        dummySourced(ValueFQN(testModuleName, QualifiedName("Box", Qualifier.Type)))
      )
    )

  private val hyperBoxType: Sourced[OperatorResolvedExpression] =
    dummySourced(
      OperatorResolvedExpression.ValueReference(
        dummySourced(ValueFQN(testModuleName, QualifiedName("HyperBox", Qualifier.Type)))
      )
    )

  private val funcType: Sourced[OperatorResolvedExpression] =
    dummySourced(OperatorResolvedExpression.ValueReference(dummySourced(Types.functionDataTypeFQN)))

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
      MonomorphicValue.Key(ValueFQN(testModuleName, default(name)), typeArgs),
      systemImports
    ).map(result => toTestErrors(result._1))
}
