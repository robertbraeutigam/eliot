package com.vanillasource.eliot.eliotc.monomorphize.processor

import cats.effect.IO
import com.vanillasource.eliot.eliotc.ProcessorTest
import com.vanillasource.eliot.eliotc.ast.processor.ASTParser
import com.vanillasource.eliot.eliotc.module.fact.{QualifiedName, Qualifier}
import com.vanillasource.eliot.eliotc.core.processor.CoreProcessor
import com.vanillasource.eliot.eliotc.module.fact.WellKnownTypes
import com.vanillasource.eliot.eliotc.ability.processor.{
  AbilityImplementationCheckProcessor,
  ModuleAbilityOverlapCheckProcessor,
  AbilityImplementationProcessor
}
import com.vanillasource.eliot.eliotc.matchdesugar.processor.MatchDesugaringProcessor
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, ValueFQN}
import com.vanillasource.eliot.eliotc.module.processor.*
import com.vanillasource.eliot.eliotc.monomorphize.fact.{GroundValue, MonomorphicValue}
import com.vanillasource.eliot.eliotc.operator.processor.OperatorResolverProcessor
import com.vanillasource.eliot.eliotc.saturate.processor.SaturatedValueProcessor
import com.vanillasource.eliot.eliotc.pos.PositionRange
import com.vanillasource.eliot.eliotc.resolve.processor.ValueResolver
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.token.Tokenizer

class MonomorphicTypeCheckTest
    extends ProcessorTest(
      Tokenizer(),
      ASTParser(),
      CoreProcessor(),
      ModuleNamesProcessor(),
      UnifiedModuleNamesProcessor(),
      ModuleValueProcessor(),
      UnifiedModuleValueProcessor(),
      ValueResolver(),
      MatchDesugaringProcessor(),
      OperatorResolverProcessor(),
      SaturatedValueProcessor(),
      AbilityImplementationProcessor(),
      AbilityImplementationCheckProcessor(),
      ModuleAbilityOverlapCheckProcessor(),
      SystemNativesProcessor(),
      DataTypeNativesProcessor(),
      UserValueNativesProcessor(),
      MonomorphicTypeCheckProcessor()
    ) {

  // --- Explicit integerLiteral constructor ---

  "explicit integerLiteral" should "type-check to the singleton Int[V, V]" in {
    // `Int` and `integerLiteral` are ambient (Phase-6), so the snippet uses them directly rather than redeclaring them.
    runForErrors("def f: Int[5, 5] = integerLiteral[5]")
      .asserting(_ shouldBe Seq.empty)
  }

  // --- Coerce[Int, Int] instance ---

  "Coerce[Int, Int] instance" should "type-check and resolve via the ability machinery" in {
    runCoerce("def test: Option[Int[0, 10]] = coerce(integerLiteral[3])")
      .asserting(_ shouldBe Seq.empty)
  }

  "check-mode Coerce insertion" should "implicitly widen Int[3, 3] to Int[0, 10]" in {
    runCoerce("def test: Int[0, 10] = integerLiteral[3]")
      .asserting(_ shouldBe Seq.empty)
  }

  it should "reject widening Int[5, 5] to the non-containing Int[0, 3]" in {
    runCoerce("def test: Int[0, 3] = integerLiteral[5]")
      .asserting(_ shouldBe Seq("Type mismatch." at "integerLiteral[5]"))
  }

  it should "still accept a definitionally equal Int[7, 7] without coercion" in {
    runCoerce("def test: Int[7, 7] = integerLiteral[7]")
      .asserting(_ shouldBe Seq.empty)
  }

  // --- Value-parameter function invoked with `[...]` (the jvm `Int` width-policy `fitsIn` pattern) ---

  "a value-parameter predicate invoked with [...]" should "type-check, the brackets only keeping the literal arguments BigInteger" in {
    runInt(
      "import eliot.lang.Bool\ndef fitsIn(lo: BigInteger, hi: BigInteger): Bool = lessThanOrEqual(lo, hi)\ndef test: Bool = fitsIn[1, 2]"
    ).asserting(_ shouldBe Seq.empty)
  }

  // --- Combine: covariant multi-candidate metavariable ---

  "Combine[Int, Int] instance" should "join two ranges via a covariant result type parameter" in {
    runCombine("def pick[A](first: A, second: A): A\ndef test: Int[3, 7] = pick(integerLiteral[3], integerLiteral[7])")
      .asserting(_ shouldBe Seq.empty)
  }

  it should "join then widen the result to an even broader expected range" in {
    runCombine("def pick[A](first: A, second: A): A\ndef test: Int[0, 20] = pick(integerLiteral[3], integerLiteral[7])")
      .asserting(_ shouldBe Seq.empty)
  }

  it should "need no combine when both candidates are the same range" in {
    runCombine("def pick[A](first: A, second: A): A\ndef test: Int[5, 5] = pick(integerLiteral[5], integerLiteral[5])")
      .asserting(_ shouldBe Seq.empty)
  }

  it should "reject combining a contravariant metavariable (taint on a function-typed parameter)" in {
    runCombine(
      "def consume(value: Int[0, 0]): Unit\ndef useTwice[A](first: A, second: A, action: Function[A, Unit]): Unit\ndef test: Unit = useTwice(integerLiteral[0], integerLiteral[10], consume)"
    ).asserting(_ shouldBe Seq("Type mismatch." at "integerLiteral[10]"))
  }

  it should "fall back to a mismatch when no Combine instance applies" in {
    runCombine("def pick[A](first: A, second: A): A\ndef test: String = pick(\"hello\", integerLiteral[3])")
      .asserting(_ shouldBe Seq("Type mismatch." at "integerLiteral[3]"))
  }

  it should "reject when the join overflows a narrower declared result type" in {
    // The join Int[3,7] does not fit the declared Int[3,5]; the result-against-declared-type check is deferred to drain
    // (after the join is known), so the join — not the first candidate Int[3,3] — is checked against Int[3,5].
    runCombine("def pick[A](first: A, second: A): A\ndef test: Int[3, 5] = pick(integerLiteral[3], integerLiteral[7])")
      .asserting(_ shouldBe Seq("Type mismatch." at "integerLiteral[7]"))
  }

  // --- Arithmetic: dependent-bounds `+` ---

  "dependent-bounds +" should "type-check Int[3, 3] + Int[4, 4] to the summed singleton Int[7, 7]" in {
    runAdd("def test: Int[7, 7] = integerLiteral[3] + integerLiteral[4]")
      .asserting(_ shouldBe Seq.empty)
  }

  it should "widen the summed result to a broader expected range" in {
    runAdd("def test: Int[0, 100] = integerLiteral[3] + integerLiteral[4]")
      .asserting(_ shouldBe Seq.empty)
  }

  it should "reject when the summed result overflows a narrower declared type" in {
    runAdd("def test: Int[6, 6] = integerLiteral[3] + integerLiteral[4]")
      .asserting(_ shouldBe Seq("Type mismatch." at "integerLiteral[3] + integerLiteral[4]"))
  }

  "dependent-bounds -" should "type-check Int[9, 9] - Int[4, 4] to the singleton Int[5, 5]" in {
    runAdd("def test: Int[5, 5] = integerLiteral[9] - integerLiteral[4]")
      .asserting(_ shouldBe Seq.empty)
  }

  it should "compute a range difference Int[5, 10] - Int[0, 3] : Int[2, 10]" in {
    runAdd("def hi: Int[5, 10]\ndef lo: Int[0, 3]\ndef test: Int[2, 10] = hi - lo")
      .asserting(_ shouldBe Seq.empty)
  }

  "dependent-bounds *" should "type-check Int[3, 3] * Int[4, 4] to the singleton Int[12, 12]" in {
    runAdd("def test: Int[12, 12] = integerLiteral[3] * integerLiteral[4]")
      .asserting(_ shouldBe Seq.empty)
  }

  it should "compute corner-product bounds Int[2, 3] * Int[4, 5] : Int[8, 15]" in {
    runAdd("def a: Int[2, 3]\ndef b: Int[4, 5]\ndef test: Int[8, 15] = a * b")
      .asserting(_ shouldBe Seq.empty)
  }

  "operator precedence" should "bind * tighter than + (1 + 2 * 3 : Int[7, 7])" in {
    runAdd("def test: Int[7, 7] = integerLiteral[1] + integerLiteral[2] * integerLiteral[3]")
      .asserting(_ shouldBe Seq.empty)
  }

  // --- Bare-literal widening: Phase-6 desugar + Phase-3 Coerce, end to end ---

  "bare literal widening" should "widen a bare value-position literal into a broader declared range" in {
    // `7` desugars to `integerLiteral[7] : Int[7, 7]` (Phase 6), then the `Coerce[Int, Int]` instance widens it
    // into `Int[0, 1000]` (Phase 3) — no explicit `integerLiteral` in the source.
    runCoerce("def test: Int[0, 1000] = 7")
      .asserting(_ shouldBe Seq.empty)
  }

  it should "reject a bare literal that does not fit the declared range" in {
    runCoerce("def test: Int[0, 3] = 5")
      .asserting(_ shouldBe Seq("Type mismatch." at "5"))
  }

  it should "widen a bare literal into a width alias (Byte)" in {
    runCoerce("def test: Byte = 5")
      .asserting(_ shouldBe Seq.empty)
  }

  it should "reject a bare literal that overflows a width alias (Byte)" in {
    runCoerce("def test: Byte = 5000")
      .asserting(_ shouldBe Seq("Type mismatch." at "5000"))
  }

  // --- Function call tests ---

  "function call" should "compile if same number of arguments" in {
    runForErrors("def f: String = b\ndef b: String")
      .asserting(_ shouldBe Seq.empty)
  }

  it should "not compile if call site has arguments, but definition doesn't" in {
    runForErrors("def f: String = b(1)\ndef b: String")
      .asserting(_ shouldBe Seq("Not a function." at "b(1)"))
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

  it should "include Expected and Actual types in description on type mismatch" in {
    runGenerator(
      "data TypeA(fieldA: TypeA)\ndata TypeB\ndef f(x: TypeA): TypeB = x",
      MonomorphicValue.Key(ValueFQN(testModuleName, default("f")), Seq.empty),
      systemImports
    ).asserting(result => result._1.map(_.description) shouldBe Seq(Seq("Expected: TypeB", "Actual:   TypeA")))
  }

  it should "render Expected and Actual function types with arrow notation" in {
    runGenerator(
      "data A\ndata B\ndef f: A = g\ndef g(a: A): B",
      MonomorphicValue.Key(ValueFQN(testModuleName, default("f")), Seq.empty),
      systemImports
    ).asserting(result => result._1.map(_.description) shouldBe Seq(Seq("Expected: A", "Actual:   A -> B")))
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

  // --- Literals ---

  "literals" should "monomorphize integer literal in body" in {
    // A bare value-position literal is now an `Int[42, 42]` singleton (Phase-6 desugar), not a `BigInteger`.
    runForErrors("def f: Int[42, 42] = 42")
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

  it should "not produce type checked results if arities mismatch" in {
    runForErrors("data A\ndef f: A = b(3)\ndef b: A")
      .asserting(_.nonEmpty shouldBe true)
  }

  it should "fail if parameter is of wrong type" in {
    runForErrors("data A\ndata B\ndef f(b: B): A = b")
      .asserting(_ shouldBe Seq("Type mismatch." at "b"))
  }

  // --- Generic types (Step 5) ---

  "generic types" should "type check when returning itself from a parameter" in {
    runForErrors("def f[A](a: A): A = a", typeArgs = Seq(intType))
      .asserting(_ shouldBe Seq.empty)
  }

  it should "type check with multiple type parameters" in {
    runForErrors("def f[A, B](a: A, b: B): A = a", typeArgs = Seq(intType, stringType))
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
      .asserting(_ shouldBe Seq("Type mismatch." at "i"))
  }

  it should "fail if forward unification to concrete types produces conflict in recursive setup" in {
    runForErrors("def id[A](a: A): A = a\ndef f(i: BigInteger, s: String): String = id(id(id(i)))")
      .asserting(_.nonEmpty shouldBe true)
  }

  it should "fail with wrong return type on generic function" in {
    runForErrors("def f[A, B](a: A, b: B): A = b", typeArgs = Seq(intType, stringType))
      .asserting(_ should have size 1)
  }

  // --- Functions without body (generic variants, Step 5) ---

  it should "be monomorphized with generic parameters" in {
    runForErrors("def f[A](a: A): A", typeArgs = Seq(intType))
      .asserting(_ shouldBe Seq.empty)
  }

  it should "be monomorphized with generic parameters and multiple arguments" in {
    runForErrors("def f[A, B](a: A, b: B): A", typeArgs = Seq(intType, stringType))
      .asserting(_ shouldBe Seq.empty)
  }

  // --- Explicit type arguments (Step 5) ---

  "explicit type arguments" should "type check when the explicit arg matches usage" in {
    runForErrors("def id[A](a: A): A = a\ndef f(s: String): String = id[String](s)")
      .asserting(_ shouldBe Seq.empty)
  }

  it should "type check with too few explicit type args by inferring the rest" in {
    runForErrors("def f2[A, B](a: A, b: B): A = a\ndef f(s: String, i: BigInteger): String = f2[String](s, i)")
      .asserting(_ shouldBe Seq.empty)
  }

  it should "type check with explicit type args and multiple type params" in {
    runForErrors(
      "def g[A, B](a: A, b: B): A = a\ndef f(s: String, i: BigInteger): String = g[String, BigInteger](s, i)"
    ).asserting(_ shouldBe Seq.empty)
  }

  it should "fail when the explicit type arg conflicts with the value argument" in {
    runForErrors("def id[A](a: A): A = a\ndef f(s: String): String = id[BigInteger](s)")
      .asserting(_ should contain("Type mismatch." at "s"))
  }

  it should "fail when the explicit type arg conflicts with the declared return type" in {
    runForErrors("def id[A](a: A): A = a\ndef i: BigInteger\ndef f(s: String): String = id[BigInteger](i)")
      .asserting(_ shouldBe Seq("Type mismatch." at "i"))
  }

  it should "fail with too many type arguments" in {
    runForErrors("def id[A](a: A): A = a\ndef f(s: String): String = id[String, String](s)")
      .asserting(_ shouldBe Seq("Not a function." at "id[String, String](s)"))
  }

  it should "fail with too few explicit type args that conflict with usage" in {
    runForErrors("def f2[A, B](a: A, b: B): A = a\ndef f(s: String, i: BigInteger): String = f2[BigInteger](s, i)")
      .asserting(_.nonEmpty shouldBe true)
  }

  it should "fail when explicit type args are in the wrong order" in {
    runForErrors(
      "def g[A, B](a: A, b: B): A = a\ndef f(s: String, i: BigInteger): String = g[BigInteger, String](s, i)"
    ).asserting(_ should contain("Type mismatch." at "i"))
  }

  it should "point type argument mismatch to the explicit type argument" in {
    runForErrors(
      "data Box[A: Type](content: String)\ndef g: String\ndef f(x: String): Box[String] = Box[BigInteger](g)"
    ).asserting(_ shouldBe Seq("Type mismatch." at "g"))
  }

  it should "type check with an applied generic type as a type argument" in {
    runForErrors("def id[A](a: A): A = a\ndata Box(s: String)\ndef f(b: Box): Box = id[Box](b)")
      .asserting(_ shouldBe Seq.empty)
  }

  // --- Multi-parameter unification (Step 5) ---

  "multi-parameter unification" should "unify on multiple parameters" in {
    runForErrors(
      "def g[A](a: A, b: A, c: A): A = a\ndef someA[A]: A\ndef f(s: String): String = g(someA, someA, s)"
    ).asserting(_ shouldBe Seq.empty)
  }

  it should "fail, if unifying on multiple parameters fail at later stage" in {
    runForErrors(
      "def g[A](a: A, b: A, c: A): A = a\ndef someA[A]: A\ndef f(i: BigInteger, s: String): String = g(someA, someA, i)"
    ).asserting(_.nonEmpty shouldBe true)
  }

  // --- Apply (Step 5) ---

  "apply" should "type check and return B" in {
    runForErrors("def f[A, B](g: Function[A, B], a: A): B = g(a)", typeArgs = Seq(intType, stringType))
      .asserting(_ shouldBe Seq.empty)
  }

  // --- Function application with generics (Step 5) ---

  "function application" should "type check generic function application" in {
    runForErrors("def id[A](a: A): A = a\ndef f: Int[42, 42] = id(42)")
      .asserting(_ shouldBe Seq.empty)
  }

  // --- Higher-kinded types (Step 6) ---

  "higher-kinded types" should "type check through single generic placeholder" in {
    runForErrors(
      "def id[A](a: A): A\ndef f[A, B, C[_, _]](c: C[A, B]): C[A, B] = id(c)",
      typeArgs = Seq(intType, stringType, funcType)
    ).asserting(_ shouldBe Seq.empty)
  }

  // Higher-kinded pattern unification is not yet supported. Unifying `?A[?B]` against `Function[Int, String]`
  // has multiple valid solutions (e.g. `?A = λx. Function[x, String], ?B = Int` or
  // `?A = λx. Function[Int, x], ?B = String`); the current unifier postpones such constraints and never solves
  // them. Previously this test passed only because the silent `GroundValue.Type` fallback in `forceAndConst`
  // masked the unresolved metas. With strict post-drain quoting the gap surfaces as a real error.
  ignore should "accept lower arities of generic parameters" in {
    runForErrors(
      "def id[B, A[_]](a: A[B]): A[B]\ndef f[A, B, C[_, _]](c: C[A, B]): C[A, B] = id(c)",
      typeArgs = Seq(intType, stringType, funcType)
    ).asserting(_ shouldBe Seq.empty)
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
      .asserting(_.nonEmpty shouldBe true)
  }

  it should "type check nested higher-kinded parameter" in {
    runForErrors(
      "data Box[A]\ndata HyperBox[A[_]]\ndef f[G[_], F[_[_]]](x: F[G]): F[G] = x",
      typeArgs = Seq(boxType, testType("HyperBox"))
    ).asserting(_ shouldBe Seq.empty)
  }

  // Injectivity decomposition: `?F[BigInt] ~ Box[BigInt]` at the call site should solve `?F := Box` without
  // needing explicit type args. Covers the `?id [x] ~ C [x]` case GHC/Scala 3 handle via injective type-application
  // decomposition.
  it should "infer higher-kinded type parameter from argument type" in {
    runForErrors(
      "data Box[A]\ndef id[F[_]](x: F[BigInteger]): F[BigInteger] = x\ndef someBox: Box[BigInteger]\ndef f: Box[BigInteger] = id(someBox)"
    ).asserting(_ shouldBe Seq.empty)
  }

  // Decomposition must cascade: `?F[?G[BigInt]] ~ Box[Maybe[BigInt]]` decomposes to `?F := Box`, then
  // `?G[BigInt] ~ Maybe[BigInt]` decomposes to `?G := Maybe`, then BigInt ~ BigInt trivially.
  it should "infer nested higher-kinded type parameters from argument type" in {
    runForErrors(
      "data Box[A]\ndata Maybe[A]\ndef id[F[_], G[_]](x: F[G[BigInteger]]): F[G[BigInteger]] = x\ndef nested: Box[Maybe[BigInteger]]\ndef f: Box[Maybe[BigInteger]] = id(nested)"
    ).asserting(_ shouldBe Seq.empty)
  }

  // Two-arg HKT decomposition: `?F[BigInt, String] ~ Function[BigInt, String]` → `?F := Function` and spines match.
  it should "infer two-argument higher-kinded type parameter from argument type" in {
    runForErrors(
      "def id[F[_, _]](x: F[BigInteger, String]): F[BigInteger, String] = x\ndef someFunc: Function[BigInteger, String]\ndef f: Function[BigInteger, String] = id(someFunc)"
    ).asserting(_ shouldBe Seq.empty)
  }

  // Decomposition is structural: `?F[BigInt] ~ Box[String]` solves `?F := Box`, then the spine pointwise unify
  // `BigInt ~ String` fails. The error must surface rather than silently succeeding via default-to-Type.
  it should "report type mismatch inside higher-kinded parameter spine" in {
    runForErrors(
      "data Box[A]\ndef id[F[_]](x: F[BigInteger]): F[BigInteger] = x\ndef someBox: Box[String]\ndef f: Box[String] = id(someBox)"
    ).asserting(_.nonEmpty shouldBe true)
  }

  // --- Explicit type restrictions (Step 6) ---

  "explicit type restrictions" should "type check with explicit Type restriction like implicit" in {
    runForErrors("def f[A: Type](a: A): A = a", typeArgs = Seq(intType))
      .asserting(_ shouldBe Seq.empty)
  }

  it should "type check with explicit Function restriction like arity syntax" in {
    runForErrors(
      "data Box[A]\ndef f[F: Function[Type, Type]](x: F[BigInteger]): F[BigInteger] = x",
      typeArgs = Seq(boxType)
    ).asserting(_ shouldBe Seq.empty)
  }

  it should "type check with explicit two-arg Function restriction" in {
    runForErrors(
      "type AlwaysString[A, B] = String\ndef f[F: Function[Type, Function[Type, Type]]](x: F[BigInteger, String]): F[BigInteger, String] = x",
      typeArgs = Seq(testType("AlwaysString"))
    ).asserting(_ shouldBe Seq.empty)
  }

  it should "type check with explicit nested higher-kinded restriction" in {
    runForErrors(
      "type AlwaysString[A, B] = String\ntype HyperAlwaysString[F[_]] = String\ndef f[G: Function[Type, Type], F: Function[Function[Type, Type], Type]](x: F[G]): F[G] = x",
      typeArgs = Seq(testType("AlwaysString"), testType("HyperAlwaysString"))
    ).asserting(_ shouldBe Seq.empty)
  }

  it should "type check with higher-kinded type that invokes its parameter" in {
    runForErrors(
      "type Identity[A] = A\ntype Apply[F[_]] = F[BigInteger]\ndef f[G: Function[Type, Type], F: Function[Function[Type, Type], Type]](x: F[G]): F[G] = x",
      typeArgs = Seq(testType("Identity"), testType("Apply"))
    ).asserting(_ shouldBe Seq.empty)
  }

  // --- Lambda type inference (Step 8) ---

  "lambda type inference" should "infer parameter type for unannotated lambda from context" in {
    runForErrors("data Foo(l: Function[Unit, String])\ndef g: String\ndef f: Foo = Foo(unit -> g)")
      .asserting(_ shouldBe Seq.empty)
  }

  it should "accept unannotated lambda with function application in body" in {
    runForErrors(
      "data Foo(l: Function[Unit, String])\ndef g(u: Unit): String\ndef f: Foo = Foo(unit -> g(unit))"
    ).asserting(_ shouldBe Seq.empty)
  }

  it should "reject annotated lambda whose param type mismatches expected function domain" in {
    runForErrors("data Foo(l: Function[String, BigInteger])\ndef f: Foo = Foo((x: BigInteger) -> 42)")
      .asserting(_ should contain("Type mismatch." at "x"))
  }

  it should "infer type parameter of generic data constructor from unannotated lambda body" in {
    // Mirrors the jvm `IO(_ -> printlnInternal(s))` shape from the HelloWorld example. Before the refactor this
    // produced `Expected: Box[Unit]` / `Actual: Box[Type]` because `Checker.forceAndConst` silently defaulted the
    // unsolved `A` meta of Box to `GroundValue.Type`, and `DataTypeNativesProcessor` fired its VNative chain
    // eagerly (using `Evaluator.semToGround`, which has the same silent fallback on metas). With strict
    // post-drain quoting and the type-constructor binding encoded as a deferred `VTopDef`, the meta is solved to
    // Unit via the inner lambda body and the whole expression type-checks cleanly.
    runForErrors(
      "data Box[A](block: Function[Unit, A])\ndef makeUnit: Unit\ndef f: Box[Unit] = Box(_ -> makeUnit)"
    ).asserting(_ shouldBe Seq.empty)
  }

  // --- Type level functions (Step 7) ---

  "type level functions" should "support non-type (value) type parameters" in {
    runForErrors(
      "data Person[S: String](name: String)\ndef f[S: String]: Person[S] = Person[S](\"\")",
      typeArgs = Seq(GroundValue.Direct("STR", stringType))
    ).asserting(_ shouldBe Seq.empty)
  }

  it should "calculate concrete literal values" in {
    // String literals (unaffected by the Phase-6 integer desugar) carry the concrete value-level equality being tested.
    runForErrors(
      "def one: String = \"x\"\ndef oneDifferently: String = \"x\"\ndef str: String\ndata Box[I: String](name: String)\ndef f: Box[one] = Box[oneDifferently](str)"
    ).asserting(_ shouldBe Seq.empty)
  }

  it should "reject calculated differing concrete literal values" in {
    runForErrors(
      "def one: String = \"x\"\ndef two: String = \"y\"\ndef str: String\ndata Box[I: String](name: String)\ndef f: Box[one] = Box[two](str)"
    ).asserting(_.nonEmpty shouldBe true)
  }

  it should "calculate concrete data values" in {
    runForErrors(
      "data Person(value: String)\ndef one: Person = Person(\"x\")\ndef oneDifferently: Person = Person(\"x\")\ndef str: String\ndata Box[I: Person](name: String)\ndef f: Box[one] = Box[oneDifferently](str)"
    ).asserting(_ shouldBe Seq.empty)
  }

  it should "reject calculated differing data values" in {
    runForErrors(
      "data Person(value: String)\ndef one: Person = Person(\"x\")\ndef two: Person = Person(\"y\")\ndef str: String\ndata Box[I: Person](name: String)\ndef f: Box[one] = Box[two](str)"
    ).asserting(_ shouldBe Seq("Type mismatch." at "str")) // TODO: attribution wrong!
  }

  it should "accept type-level function calls that are not Type types" in {
    runForErrors(
      "def g(x: String): String = x\ndata Box[X: String](value: String)\ndef f[G: String](value: String): Box[g(G)] = Box[G](value)",
      typeArgs = Seq(GroundValue.Direct("STR", stringType))
    ).asserting(_ shouldBe Seq.empty)
  }

  // --- Opaque modifier ---

  "opaque modifier" should "keep a plain (transparent) alias definitionally equal to its body" in {
    runForErrors("type Id[A] = A\ndef s: String\ndef f: Id[String] = s")
      .asserting(_ shouldBe Seq.empty)
  }

  it should "make an opaque alias NOT definitionally equal to its body" in {
    // With the body hidden, `Box[String]` stays a stuck reference and does not unfold to `String`, so a `String` is no
    // longer assignable to it.
    runForErrors("opaque type Box[A] = A\ndef s: String\ndef f: Box[String] = s")
      .asserting(_ shouldBe Seq("Type mismatch." at "s"))
  }

  it should "keep an opaque alias definitionally equal to itself" in {
    runForErrors("opaque type Box[A] = A\ndef s: Box[String]\ndef f: Box[String] = s")
      .asserting(_ shouldBe Seq.empty)
  }

  it should "merge distinct type arguments of a transparent alias that ignores them" in {
    // `W[1]` and `W[2]` both unfold to `String`, so `W[1]` is accepted where `W[2]` is expected — the unsound collapse
    // an opaque barrier must prevent for representation-bearing types.
    runForErrors("type W[N: BigInteger] = String\ndef f(x: W[1]): W[2] = x")
      .asserting(_ shouldBe Seq.empty)
  }

  it should "keep distinct type arguments of an opaque alias distinct" in {
    // The opaque barrier keeps `W[1]` and `W[2]` separate even though both bodies are `String` — the soundness property
    // that lets a platform `opaque type Int[MIN, MAX] = <repr>` map ranges to representations without conflating them.
    runForErrors("opaque type W[N: BigInteger] = String\ndef f(x: W[1]): W[2] = x")
      .asserting(_ shouldBe Seq("Type mismatch." at "x"))
  }

  // --- Recursion (Step 9) ---

  "recursion" should "handle direct recursion without infinite loop" in {
    import scala.concurrent.duration.*
    runForErrors("def f: Function[BigInteger, BigInteger] = f")
      .timeout(1.seconds)
      .asserting(_ shouldBe Seq.empty)
  }

  it should "handle mutual recursion without infinite loop" in {
    import scala.concurrent.duration.*
    runForErrors("def f: Function[BigInteger, BigInteger] = g\ndef g: Function[BigInteger, BigInteger] = f")
      .timeout(1.seconds)
      .asserting(_ shouldBe Seq.empty)
  }

  // --- Implicit / inferred generic inputs: bare `Int` parameters (W1) ---

  "implicit generic inputs (W1)" should "let a caller pass a concrete Int into a bare-Int parameter" in {
    // `Int` is `auto`-marked, so `isEven`'s bare `Int` parameter generalizes; the caller infers the bounds from `b`.
    runW1("type Bool\ndef isEven(x: Int): Bool\ndef main(b: Int[0, 255]): Bool = isEven(b)")
      .asserting(_ shouldBe Seq.empty)
  }

  it should "let a bodied consumer with a bare Int parameter type-check at a call" in {
    runW1("def f(x: Int): String = \"x\"\ndef main(b: Int[0, 255]): String = f(b)")
      .asserting(_ shouldBe Seq.empty)
  }

  it should "monomorphize the saturated consumer itself at concrete bounds" in {
    // The synthesized binders are ordinary generic parameters, so `f` monomorphizes given the two bound type arguments.
    runW1(
      "def f(x: Int): String = \"x\"",
      name = "f",
      typeArgs = Seq(GroundValue.Direct(BigInt(0), intType), GroundValue.Direct(BigInt(255), intType))
    ).asserting(_ shouldBe Seq.empty)
  }

  it should "let a bare Int parameter coexist with a fully-applied IO return" in {
    runW1("def store(x: Int): IO[Unit]\ndef main(b: Int[0, 255]): IO[Unit] = store(b)")
      .asserting(_ shouldBe Seq.empty)
  }

  it should "give two bare Int parameters independent ranges" in {
    // Shared binders would force `Int[0,255]` and `Int[0,1000]` to unify and conflict; independence makes this check.
    runW1(
      "type Bool\ndef add2(x: Int, y: Int): Bool\ndef main(b: Int[0, 255], c: Int[0, 1000]): Bool = add2(b, c)"
    ).asserting(_ shouldBe Seq.empty)
  }

  it should "still reject a bare reference to an unmarked (non-auto) parameterized type" in {
    // `IO`'s parameter is not `auto`, so bare `IO` is not saturated and the ordinary check rejects the call.
    runW1("type Bool\ndef bad(x: IO): Bool\ndef main(thing: IO[Unit]): Bool = bad(thing)")
      .asserting(_.nonEmpty shouldBe true)
  }

  it should "leave an explicit Int[0, 255] parameter unchanged" in {
    runW1("def f(x: Int[0, 255]): String = \"x\"\ndef main(b: Int[0, 255]): String = f(b)")
      .asserting(_ shouldBe Seq.empty)
  }

  // --- Implicit / inferred generic data fields: bare `Int` fields (W2) ---

  "implicit generic data fields (W2)" should "construct a record carrying a bare Int field's bounds" in {
    // `Counter`'s bare `Int` field generalizes the data type to `Counter[lo, hi]`; constructing at `Int[0, 255]`
    // produces `Counter[0, 255]`.
    runForErrors("data Counter(n: Int)\ndef test(b: Int[0, 255]): Counter[0, 255] = Counter(b)", name = "test")
      .asserting(_ shouldBe Seq.empty)
  }

  it should "reject constructing a record at bounds the field argument does not satisfy" in {
    runForErrors("data Counter(n: Int)\ndef test(b: Int[0, 1000]): Counter[0, 255] = Counter(b)", name = "test")
      .asserting(_.nonEmpty shouldBe true)
  }

  it should "give a mixed-field record four independent binders" in {
    runForErrors(
      "data Pair(a: Int, b: Int)\ndef test(x: Int[0, 255], y: Int[0, 1000]): Pair[0, 255, 0, 1000] = Pair(x, y)",
      name = "test"
    ).asserting(_ shouldBe Seq.empty)
  }

  it should "leave an explicit Int[0, 255] field unchanged" in {
    runForErrors("data SmallInt(v: Int[0, 255])\ndef test(b: Int[0, 255]): SmallInt = SmallInt(b)", name = "test")
      .asserting(_ shouldBe Seq.empty)
  }

  it should "access a record field with the field's bounds" in {
    runMatch("data Counter(n: Int)\ndef test(c: Counter[0, 255]): Int[0, 255] = n(c)")
      .asserting(_ shouldBe Seq.empty)
  }

  it should "round-trip a record field through a match with the field's bounds" in {
    runMatch("data Counter(n: Int)\ndef test(c: Counter[0, 255]): Int[0, 255] = c match { case Counter(x) -> x }")
      .asserting(_ shouldBe Seq.empty)
  }

  // Follow-up 1 (multi-constructor unions with bare `auto` fields) is deferred as an open design point; these two
  // lock in that the deferral is *fail-safe*. A union constructor's bare field still generalizes per-occurrence (W1),
  // so constructing one type-checks — but the union type does not grow, so it stays bare `Maybe` (the bound is not
  // tracked through the union)...
  it should "construct a multi-constructor union with a bare Int field as the bare union type" in {
    runForErrors("data Maybe = Nothing | Just(value: Int)\ndef test(b: Int[0, 255]): Maybe = Just(b)", name = "test")
      .asserting(_ shouldBe Seq.empty)
  }

  // ...and supplying bounds to it explicitly (`Maybe[0, 255]`) is rejected loudly, never silently accepted.
  it should "reject supplying generic bounds to an un-grown multi-constructor union type" in {
    runForErrors("data Maybe = Nothing | Just(value: Int)\ndef test(b: Int[0, 255]): Maybe[0, 255] = Just(b)", name = "test")
      .asserting(_.nonEmpty shouldBe true)
  }

  it should "type-level match over an auto-bounded record, binding both synthesized bounds" in {
    // The grown `Counter[lo, hi]` makes the `typeMatch` matcher's handler `Function[BigInteger, BigInteger, R]`, so a
    // type-level `case Counter[lo, hi]` binds both bounds (it stayed `Function[Unit, R]` before this follow-up).
    runMatch(
      "data Counter(n: Int)\ndef test(t: Type): String = t match { case Counter[lo, hi] -> \"c\"\ncase _ -> \"n\" }"
    ).asserting(_ shouldBe Seq.empty)
  }

  // --- Implicit / inferred generic calculated returns: bare `Int` returns (W3) ---

  "calculated return positions (W3)" should "calculate a bare Int return from the body and observe it at a caller" in {
    // `double`'s bare `Int` return is *calculated*: `x + x` at `Int[0, 255]` yields `Int[0, 510]`, which the caller
    // reads off `double`'s monomorphized signature rather than the (under-applied) source return.
    runInt("def double(x: Int): Int = x + x\ndef test(b: Int[0, 255]): Int[0, 510] = double(b)")
      .asserting(_ shouldBe Seq.empty)
  }

  it should "monomorphize a bare-Int-return producer itself at concrete bounds" in {
    // The callee side in isolation: monomorphizing `double` at `[0, 255]` fills the calculated return from the body.
    runInt(
      "def double(x: Int): Int = x + x",
      name = "double",
      typeArgs = Seq(GroundValue.Direct(BigInt(0), intType), GroundValue.Direct(BigInt(255), intType))
    ).asserting(_ shouldBe Seq.empty)
  }

  it should "reject a calculated return that does not fit a narrower declared type" in {
    // The tightest calculated return `Int[0, 510]` does not `Coerce` into `Int[0, 100]`.
    runInt("def double(x: Int): Int = x + x\ndef test(b: Int[0, 255]): Int[0, 100] = double(b)")
      .asserting(_.nonEmpty shouldBe true)
  }

  it should "ground a chained calculated return through both instantiations" in {
    // `double(double(b))` grounds the inner call at `[0, 255]` (→ `Int[0, 510]`) and the outer at `[0, 510]`
    // (→ `Int[0, 1020]`); each producer's monomorphized return drives the next.
    runInt("def double(x: Int): Int = x + x\ndef test(b: Int[0, 255]): Int[0, 1020] = double(double(b))")
      .asserting(_ shouldBe Seq.empty)
  }

  it should "widen a bare calculated return into a broader declared type via Coerce" in {
    // Bare return = the tightest calculated type (`Int[0, 510]`); the explicit caller annotation `Int[0, 1000]` is a
    // wider published contract, reached by the existing check-mode `Coerce`.
    runInt("def double(x: Int): Int = x + x\ndef test(b: Int[0, 255]): Int[0, 1000] = double(b)")
      .asserting(_ shouldBe Seq.empty)
  }

  it should "leave an explicit return (not calculated) widened from the body, unchanged" in {
    // An explicit `Int[0, 1000]` return is *not* calculated: the body `x + x` (`Int[0, 510]`) is widened to it via
    // `Coerce` in the callee, and the caller reads `Int[0, 1000]` directly.
    runInt("def double(x: Int): Int[0, 1000] = x + x\ndef test(b: Int[0, 255]): Int[0, 1000] = double(b)")
      .asserting(_ shouldBe Seq.empty)
  }

  it should "calculate a bare data return (saturated record) from the body" in {
    // `mk`'s bare `Counter` return is calculated: constructing `Counter(n)` at `Int[0, 255]` yields `Counter[0, 255]`,
    // which the caller observes — a producer returning a W2-grown `data`.
    runForErrors(
      "data Counter(n: Int)\ndef mk(n: Int): Counter = Counter(n)\ndef test(b: Int[0, 255]): Counter[0, 255] = mk(b)",
      name = "test"
    ).asserting(_ shouldBe Seq.empty)
  }

  "calculated return limits (W4)" should "report a self-recursive calculated return instead of dead-locking" in {
    // Limit 1: `loop`'s bare `Int` return is calculated from its body, but the body re-references `loop`, so reading its
    // monomorphized return would re-enter the in-progress computation (a fact-cache dead-lock). Detected and reported.
    runInt(
      "def loop(x: Int): Int = loop(x)",
      name = "loop",
      typeArgs = Seq(GroundValue.Direct(BigInt(0), intType), GroundValue.Direct(BigInt(255), intType))
    ).asserting(_.map(_.message) shouldBe Seq("Cannot calculate the return type of recursive value 'loop'."))
  }

  it should "report a value-dependent (non-stabilising) calculated return" in {
    // Limit 1, the growth flavour: each recursive call widens the bound (`x + x`), so the chain never stabilises. The
    // repeated FQN on the active request chain is the recursion signal even though no key ever repeats exactly.
    runInt(
      "def grow(x: Int): Int = grow(x + x)",
      name = "grow",
      typeArgs = Seq(GroundValue.Direct(BigInt(0), intType), GroundValue.Direct(BigInt(255), intType))
    ).asserting(_.map(_.message) shouldBe Seq("Cannot calculate the return type of recursive value 'grow'."))
  }

  it should "report a mutually-recursive calculated return" in {
    // Limit 1, mutual flavour: `f`'s return needs `g`'s, whose return needs `f`'s — `f` reappears on the active chain.
    runInt(
      "def f(x: Int): Int = g(x)\ndef g(x: Int): Int = f(x)",
      name = "f",
      typeArgs = Seq(GroundValue.Direct(BigInt(0), intType), GroundValue.Direct(BigInt(255), intType))
    ).asserting(_.map(_.message) should contain("Cannot calculate the return type of recursive value 'f'."))
  }

  it should "report a calculated return on a body-less abstract declaration" in {
    // Limit 5: `readByte` is abstract (no body) but its bare `Int` return is calculated — there is nothing to calculate
    // it from, and an output position cannot quantify it, so the bound must be stated explicitly. Reported at the
    // definition, not deferred to a confusing use-site `Type` mismatch.
    runInt(
      "def readByte(x: Int): Int",
      name = "readByte",
      typeArgs = Seq(GroundValue.Direct(BigInt(0), intType), GroundValue.Direct(BigInt(255), intType))
    ).asserting(
      _.map(_.message) shouldBe Seq(
        "Abstract declaration 'readByte' must state its return type explicitly; there is no body to calculate it from."
      )
    )
  }

  it should "observe a no-argument calculated-return producer referenced by name" in {
    // Deferred W3 item 1: `five`'s bare `Int` return is calculated (`5 : Int[5, 5]`); referencing it by name in `y`
    // (no application) now resolves that return at the read instead of leaking the `Type` placeholder into a mismatch.
    runInt("def five: Int = 5\ndef y: Int[5, 5] = five", name = "y").asserting(_ shouldBe Seq.empty)
  }

  it should "reject a no-argument calculated-return producer that does not fit the declared type" in {
    // `five`'s calculated `Int[5, 5]` does not `Coerce` into `Int[0, 3]`, so the by-name read is still checked.
    runInt("def five: Int = 5\ndef y: Int[0, 3] = five", name = "y").asserting(_.nonEmpty shouldBe true)
  }

  it should "report a self-recursive no-argument calculated-return producer" in {
    // The read-site resolution shares the recursion guard: `loopv` referencing itself is detected, not dead-locked.
    runInt("def loopv: Int = loopv", name = "loopv")
      .asserting(_.map(_.message) shouldBe Seq("Cannot calculate the return type of recursive value 'loopv'."))
  }

  it should "still accept a body-less abstract declaration with an explicit return type" in {
    // The guard targets only *calculated* (bare) returns; an explicit abstract signature (the platform-layer norm) is
    // unaffected — it type-checks as a signature with no body.
    runInt(
      "def readByte(x: Int): Int[0, 255]",
      name = "readByte",
      typeArgs = Seq(GroundValue.Direct(BigInt(0), intType), GroundValue.Direct(BigInt(255), intType))
    ).asserting(_ shouldBe Seq.empty)
  }

  private def dummySourced[T](v: T) = Sourced[T](file, PositionRange.zero, v)

  private val intType: GroundValue =
    GroundValue.Structure(WellKnownTypes.bigIntFQN, Seq.empty, GroundValue.Type)

  private val stringType: GroundValue =
    GroundValue.Structure(WellKnownTypes.stringFQN, Seq.empty, GroundValue.Type)

  private val funcType: GroundValue =
    GroundValue.Structure(WellKnownTypes.functionDataTypeFQN, Seq.empty, GroundValue.Type)

  private val boxType: GroundValue = testType("Box")

  private def testType(name: String): GroundValue =
    GroundValue.Structure(
      ValueFQN(testModuleName, QualifiedName(name, Qualifier.Type)),
      Seq.empty,
      GroundValue.Type
    )

  private def runForErrors(
      source: String,
      name: String = "f",
      typeArgs: Seq[GroundValue] = Seq.empty
  ): IO[Seq[TestError]] =
    runGenerator(
      source,
      MonomorphicValue.Key(ValueFQN(testModuleName, default(name)), typeArgs),
      systemImports
    ).map(result => toTestErrors(result._1))

  /** Like [[systemImports]] but with `PatternMatch`/`TypeMatch` declared as real abilities (the inherited stubs are
    * empty), so a surface `match` / field accessor — which resolves to the auto-generated `handleCases` implementation —
    * type-checks. Used by the W2 access/match tests.
    */
  private val matchImports: Seq[SystemImport] = systemImports.map {
    case SystemImport("PatternMatch", _) =>
      SystemImport("PatternMatch", "ability PatternMatch[T] {\ntype Cases[R]\ndef handleCases[R](value: T, cases: Cases[R]): R\n}")
    case SystemImport("TypeMatch", _)    =>
      SystemImport(
        "TypeMatch",
        "ability TypeMatch[T] {\ntype Fields[R]\ndef typeMatch[R](value: Type, matched: Fields[R], notMatched: Function[Unit, R]): R\n}"
      )
    case other                           => other
  }

  private def runMatch(source: String, name: String = "test"): IO[Seq[TestError]] =
    runGenerator(
      source,
      MonomorphicValue.Key(ValueFQN(testModuleName, default(name)), Seq.empty),
      matchImports
    ).map(result => toTestErrors(result._1))

  /** Like the ambient [[systemImports]] but with a *parameterized* `IO[A]` (its parameter is deliberately not `auto`),
    * so the W1 tests can exercise both a fully-applied `IO[Unit]` and the bare-`IO` guardrail. `Bool` is declared
    * locally in each snippet (it is not an ambient module).
    */
  private val w1Imports: Seq[SystemImport] = systemImports.map {
    case SystemImport("IO", _) => SystemImport("IO", "type IO[A]")
    case other                 => other
  }

  private def runW1(
      source: String,
      name: String = "main",
      typeArgs: Seq[GroundValue] = Seq.empty
  ): IO[Seq[TestError]] =
    runGenerator(
      source,
      MonomorphicValue.Key(ValueFQN(testModuleName, default(name)), typeArgs),
      w1Imports
    ).map(result => toTestErrors(result._1))

  /** Imports providing the full ambient `Int` environment for the `Coerce`/`Combine`/arithmetic tests. As of the
    * Phase-6 literal desugar `Int` and `Runtime` are auto-imported (`defaultSystemModules`), so — unlike before — the
    * `Int` type, its parametric range-widening `Coerce[Int, Int]` instance, the `Combine[Int, Int]` instance, the
    * `nativeWiden`, the `+`/`-`/`*` operators and the `integerLiteral` constructor live in the *ambient* `Int`/`Runtime`
    * modules rather than in a prelude prepended to the Test module. The module names mirror the real `eliot.lang.*`
    * layout so the compile-time natives (`&&`/`fold`/`lessThanOrEqual`/`min`/`max`/`add`/`subtract`/`multiply*`) bind to
    * their well-known FQNs.
    */
  private val intImports: Seq[SystemImport] = Seq(
    SystemImport("Function", "type Function[A, B]\ndef apply[A, B](f: Function[A, B], a: A): B"),
    SystemImport("Type", "type Type"),
    SystemImport(
      "BigInteger",
      "import eliot.lang.Bool\ntype BigInteger\ndef lessThanOrEqual(a: BigInteger, b: BigInteger): Bool\ndef min(a: BigInteger, b: BigInteger): BigInteger\ndef max(a: BigInteger, b: BigInteger): BigInteger\ndef add(a: BigInteger, b: BigInteger): BigInteger\ndef subtract(a: BigInteger, b: BigInteger): BigInteger\ndef multiplyMin(a: BigInteger, b: BigInteger, c: BigInteger, d: BigInteger): BigInteger\ndef multiplyMax(a: BigInteger, b: BigInteger, c: BigInteger, d: BigInteger): BigInteger"
    ),
    SystemImport("Unit", "type Unit"),
    SystemImport("String", "type String"),
    SystemImport("IO", "type IO"),
    SystemImport("PatternMatch", ""),
    SystemImport("TypeMatch", ""),
    SystemImport(
      "Bool",
      "type Bool\ndef true: Bool\ndef false: Bool\ninfix def &&(a: Bool, b: Bool): Bool\ndef fold[A](condition: Bool, whenTrue: A, whenFalse: A): A"
    ),
    SystemImport("Option", "type Option[A]\ndef some[A](value: A): Option[A]\ndef none[A]: Option[A]"),
    SystemImport("Coerce", "import eliot.lang.Option\nability Coerce[From, To] { def coerce(value: From): Option[To] }"),
    SystemImport("Combine", "ability Combine[A, B] { type Combined }"),
    SystemImport(
      "Int",
      """import eliot.lang.Bool
        |import eliot.lang.Coerce
        |import eliot.lang.Combine
        |import eliot.lang.Option
        |type Int[auto MIN: BigInteger, auto MAX: BigInteger]
        |type Byte = Int[-128, 127]
        |def nativeWiden[Smin: BigInteger, Smax: BigInteger, Tmin: BigInteger, Tmax: BigInteger](value: Int[Smin, Smax]): Int[Tmin, Tmax]
        |implement[Smin, Smax, Tmin, Tmax] Coerce[Int[Smin, Smax], Int[Tmin, Tmax]] { def coerce(value: Int[Smin, Smax]): Option[Int[Tmin, Tmax]] = fold(lessThanOrEqual(Tmin, Smin) && lessThanOrEqual(Smax, Tmax), some(nativeWiden(value)), none) }
        |implement[Amin, Amax, Bmin, Bmax] Combine[Int[Amin, Amax], Int[Bmin, Bmax]] { type Combined = Int[min(Amin, Bmin), max(Amax, Bmax)] }
        |infix left
        |def +[LMin: BigInteger, LMax: BigInteger, RMin: BigInteger, RMax: BigInteger](left: Int[LMin, LMax], right: Int[RMin, RMax]): Int[add(LMin, RMin), add(LMax, RMax)]
        |infix left at +
        |def -[LMin: BigInteger, LMax: BigInteger, RMin: BigInteger, RMax: BigInteger](left: Int[LMin, LMax], right: Int[RMin, RMax]): Int[subtract(LMin, RMax), subtract(LMax, RMin)]
        |infix left above +
        |def *[LMin: BigInteger, LMax: BigInteger, RMin: BigInteger, RMax: BigInteger](left: Int[LMin, LMax], right: Int[RMin, RMax]): Int[multiplyMin(LMin, LMax, RMin, RMax), multiplyMax(LMin, LMax, RMin, RMax)]
        |""".stripMargin
    ),
    SystemImport("Runtime", ProcessorTest.runtimeStubContent)
  )

  /** Imports for the snippet that reference `coerce`/`Option` by name (`Int` itself is ambient). */
  private val intPrelude: String =
    "import eliot.lang.Coerce\nimport eliot.lang.Option\n"

  private def runInt(
      source: String,
      name: String = "test",
      typeArgs: Seq[GroundValue] = Seq.empty
  ): IO[Seq[TestError]] =
    runGenerator(
      intPrelude + source,
      MonomorphicValue.Key(ValueFQN(testModuleName, default(name)), typeArgs),
      intImports
    ).map(result => toTestErrors(result._1))

  private def runCoerce(source: String, name: String = "test"): IO[Seq[TestError]]  = runInt(source, name)
  private def runCombine(source: String, name: String = "test"): IO[Seq[TestError]] = runInt(source, name)
  private def runAdd(source: String, name: String = "test"): IO[Seq[TestError]]     = runInt(source, name)
}
