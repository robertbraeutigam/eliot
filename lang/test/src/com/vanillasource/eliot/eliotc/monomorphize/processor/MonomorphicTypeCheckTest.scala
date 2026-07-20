package com.vanillasource.eliot.eliotc.monomorphize.processor

import cats.effect.IO
import com.vanillasource.eliot.eliotc.ProcessorTest
import com.vanillasource.eliot.eliotc.module.fact.{QualifiedName, Qualifier}
import com.vanillasource.eliot.eliotc.module.fact.WellKnownTypes
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, ValueFQN}
import com.vanillasource.eliot.eliotc.monomorphize.fact.{GroundValue, MonomorphicExpression, MonomorphicValue}
import com.vanillasource.eliot.eliotc.plugin.LangProcessors
import com.vanillasource.eliot.eliotc.pos.PositionRange
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.stdlib.plugin.StdlibNativesProcessor

// The stdlib-layer arithmetic/`&&`/`lessThanOrEqual` natives backing `Int`'s dependent bounds live in
// `StdlibNativesProcessor`, so this monomorphization suite composes it onto `LangProcessors` and registers its native
// label so the binding merger consults it. Each name has at most one native answer (disjoint suppliers); the user
// supplier is the fallback, so the merger reads the right reduction with no ordering.
class MonomorphicTypeCheckTest
    extends ProcessorTest(
      (LangProcessors(
        systemModules = ProcessorTest.coreAmbientModules,
        extraNativeBindingLabels = Seq(StdlibNativesProcessor.stdlibLabel)
      ) :+ StdlibNativesProcessor())*
    ) {

  // --- Explicit integerLiteral constructor ---

  "explicit integerLiteral" should "type-check to the singleton Int[V, V]" in {
    // `Int` and `integerLiteral` are ambient (Phase-6), so the snippet uses them directly rather than redeclaring them.
    runForErrors("def f: Int[5, 5] = integerLiteral[5]")
      .asserting(_ shouldBe Seq.empty)
  }

  // --- Coerce[Int, Int] instance ---

  "Coerce[Int, Int] instance" should "type-check and resolve via the ability machinery" in {
    runCoerce("def test: Int[0, 10] = coerce(integerLiteral[3])")
      .asserting(_ shouldBe Seq.empty)
  }

  // The check-mode `Coerce` widening the checker once inserted here was removed at Step 7a
  // (`docs/bounds-as-refinements.md`): `Int == Int` means no widening is needed or possible. What survives is plain
  // definitional equality — a non-containing range is now an ordinary mismatch, an equal range passes.
  "definitional equality on Int ranges (no Coerce)" should "reject Int[5, 5] against the non-containing Int[0, 3]" in {
    runCoerce("def test: Int[0, 3] = integerLiteral[5]")
      .asserting(_ shouldBe Seq("Type mismatch." at "integerLiteral[5]"))
  }

  it should "still accept a definitionally equal Int[7, 7] without coercion" in {
    runCoerce("def test: Int[7, 7] = integerLiteral[7]")
      .asserting(_ shouldBe Seq.empty)
  }

  it should "report a single mismatch for a cross-constructor mismatch (no spurious Coerce diagnostic)" in {
    // The failed `Coerce[String, Int[..]]` probe must stay silent: the ability machinery records the failed outcome
    // on the fact instead of erroring, and a probe (unlike a demand) reads a failed outcome as a plain decline. Before
    // outcome-carrying resolution this emitted an extra "does not implement ability 'Coerce'" against the `Coerce`
    // declaration itself.
    runCoerce("def s: String\ndef test: Int[0, 10] = s")
      .asserting(_ shouldBe Seq("Type mismatch." at "s"))
  }

  // --- Value-parameter function invoked with `[...]` (the jvm `Int` width-policy `fitsIn` pattern) ---

  "a value-parameter predicate invoked with [...]" should "type-check, the brackets only keeping the literal arguments BigInteger" in {
    runInt(
      "import eliot.lang.Bool\nimport eliot.lang.Compare\ndef fitsIn(lo: BigInteger, hi: BigInteger): Bool = lessThanOrEqual(lo, hi)\ndef test: Bool = fitsIn[1, 2]"
    ).asserting(_ shouldBe Seq.empty)
  }

  // --- Two distinct contributions to a covariant result type parameter ---
  //
  // The `Combine` branch-join that once joined divergent contributions into their least-upper-bound was removed with
  // the refinement channel's flag day (`docs/bounds-as-refinements.md` Step 7b): `Int == Int` makes the join a no-op,
  // so a second, definitionally-unequal contribution is now an ordinary first-candidate-wins mismatch. What survives
  // is exactly that mismatch behaviour.

  "a covariant result type parameter" should "need no combine when both candidates are the same range" in {
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

  // --- Arithmetic: dependent-bounds `+` ---

  "dependent-bounds +" should "type-check Int[3, 3] + Int[4, 4] to the summed singleton Int[7, 7]" in {
    runAdd("def test: Int[7, 7] = integerLiteral[3] + integerLiteral[4]")
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

  // --- Bare-literal typing: the Phase-6 `n` ⤳ `integerLiteral[n] : Int[n, n]` desugar. The Phase-3 `Coerce` widening
  //     that once broadened a bare literal into a declared range was removed at Step 7a, so a non-equal declared range
  //     now mismatches. ---

  "a bare value-position literal" should "reject a declared range it does not fit" in {
    runCoerce("def test: Int[0, 3] = 5")
      .asserting(_ shouldBe Seq("Type mismatch." at "5"))
  }

  it should "reject overflowing a width alias (Byte)" in {
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

  // --- Immediately-applied lambdas (the `let` form a non-effectful block `val`/statement lowers to) ---

  "an immediately-applied unannotated lambda" should "infer the binder type from the argument" in {
    runForErrors("data A\ndef mk: A\ndef f: A = (x -> x)(mk)")
      .asserting(_ shouldBe Seq.empty)
  }

  it should "bind the value so the body can use it at the inferred type" in {
    runForErrors("data A\ndata B\ndef mk: A\ndef use(a: A): B\ndef f: B = (x -> use(x))(mk)")
      .asserting(_ shouldBe Seq.empty)
  }

  it should "push the expected return type into the body, rejecting a mismatch" in {
    runForErrors("data A\ndata B\ndef mk: A\ndef f: B = (x -> x)(mk)")
      .asserting(_ shouldBe Seq("Type mismatch." at "x"))
  }

  it should "type-check a chain of immediately-applied lambdas (a multi-statement block)" in {
    runForErrors("data A\ndef mk: A\ndef f: A = (ignored -> (x -> x)(mk))(mk)")
      .asserting(_ shouldBe Seq.empty)
  }

  "an immediately-applied annotated lambda" should "type-check via the annotation" in {
    runForErrors("data A\ndef mk: A\ndef f: A = (x: A -> x)(mk)")
      .asserting(_ shouldBe Seq.empty)
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
      .asserting(_ shouldBe Seq("Type mismatch." at "id(i)"))
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
      .asserting(_ shouldBe Seq("Type mismatch." at "id[BigInteger](i)"))
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

  it should "anchor a type argument mismatch at the whole construction" in {
    runForErrors(
      "data Box[A: Type](content: String)\ndef g: String\ndef f(x: String): Box[String] = Box[BigInteger](g)"
    ).asserting(_ shouldBe Seq("Type mismatch." at "Box[BigInteger](g)"))
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

  // Two-arg HKT carrier over the `Function` type: a *current inference limitation*, surfaced (not silenced) by the F1
  // fail-safe fallback. `Function[BigInt, String]` normalises to a `VPi`, and the pattern unifier deliberately does not
  // injectivity-decompose `?F[A, B] ~ Function[A, B]` (see `CarrierKindChecker.verifyCarrierKinds`: a carrier postponed
  // against a non-rigid `VPi` is "higher-order unification the pattern unifier cannot solve"). So `?F` stays unsolved,
  // defaults to `VType`, and the later `F[BigInt, String]` applies type arguments to that non-applicable head. Before
  // F1, `applyValue` silently returned the argument — the program "type-checked" but with a nonsense `BigInteger[String]`
  // body type (a masked miscompile). F1 makes that a loud stuck form, so the use now surfaces `Cannot resolve type.` at
  // the use site rather than accepting a wrong typing. (An analogous `data`-carrier case — `?F := Box`, a rigid
  // `VTopDef` — *does* decompose and type-check; see the single-arg HKT cases above.) The postponed `?F[..] ~ Function[..]`
  // constraints do *not* reach the `flushPostponed` mismatch backstop: once `?F` defaults to `VType`, `?F[..]` reads back
  // as a `$bad-apply` head, which that flush triages as benign (this precise `Cannot resolve type.` owns the case).
  it should "surface (not silently miscompile) a two-arg HKT carrier over the Function type" in {
    runForErrors(
      "def id[F[_, _]](x: F[BigInteger, String]): F[BigInteger, String] = x\ndef someFunc: Function[BigInteger, String]\ndef f: Function[BigInteger, String] = id(someFunc)"
    ).asserting(_.map(_.message) shouldBe Seq("Cannot resolve type."))
  }

  // Decomposition is structural: `?F[BigInt] ~ Box[String]` solves `?F := Box`, then the spine pointwise unify
  // `BigInt ~ String` fails. The error must surface rather than silently succeeding via default-to-Type.
  it should "report type mismatch inside higher-kinded parameter spine" in {
    runForErrors(
      "data Box[A]\ndef id[F[_]](x: F[BigInteger]): F[BigInteger] = x\ndef someBox: Box[String]\ndef f: Box[String] = id(someBox)"
    ).asserting(_.nonEmpty shouldBe true)
  }

  // --- Carrier-meta kind soundness (effects M0, gap #2) ---
  // A `[F[_]]` instantiation meta must respect its `Type -> Type` kind: the unifier's direct empty-spine solve would
  // otherwise equate it with a proper type. `verifyCarrierKinds` (post-drain) rejects both wrong-kind shapes below.

  // `bad`'s signature uses the `Type -> Type` parameter `F` as a proper value type (`x: F`). Inferring `?F := Box[String]`
  // (kind `Type`) at the call must be rejected — the `[F[_]]` kind says `?F` ranges over `Type -> Type` constructors.
  it should "reject a higher-kinded carrier inferred as a fully-applied proper type" in {
    runForErrors(
      "data Box[A]\ndef bad[F[_]](x: F): F = x\ndef someBox: Box[String]\ndef f: Box[String] = bad(someBox)"
    ).asserting(_.nonEmpty shouldBe true)
  }

  // `?F[String] ~ String` has no injective type-constructor solution (only a non-injective constant lambda would fit,
  // which a carrier never is). It must be rejected rather than silently dropped from the postponement queue.
  // The doomed `?F[String] ~ String` postponement with a FITTING payload now legitimately resolves: the pure-boundary
  // Id defaulting (`EffectLifter.tryIdDefault`) solves `?F := Id` and unwraps with `runId` (end-to-end coverage in the
  // jvm `ExamplesIntegrationTest` pure-`if..else` cases, where the real `Effect[Id]` instance resolves). The rejection
  // remains for a payload that does not fit the rigid pure return — the defaulting speculates and declines there.
  it should "reject a higher-kinded carrier whose payload does not fit the rigid pure return" in {
    runForErrors(
      "def id[F[_]](x: F[String]): F[String] = x\ndef someString: String\ndef f: BigInteger = id(someString)"
    ).asserting(_.nonEmpty shouldBe true)
  }

  it should "default a higher-kinded carrier with a fitting payload to the Id carrier at a pure return" in {
    runForErrors(
      "def id[F[_]](x: F[String]): F[String] = x\ndef someString: String\ndef f: String = id(someString)"
    ).asserting(_ shouldBe Seq.empty)
  }

  // The positive direction must still hold: `?F[String] ~ Box[String]` decomposes to the well-kinded `?F := Box`.
  it should "accept a higher-kinded carrier inferred as a Type -> Type constructor" in {
    runForErrors(
      "data Box[A]\ndef id[F[_]](x: F[String]): F[String] = x\ndef someBox: Box[String]\ndef f: Box[String] = id(someBox)"
    ).asserting(_ shouldBe Seq.empty)
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
    // Mirrors the jvm `IO(_ -> printLineInternal(s))` shape from the HelloWorld example. Before the refactor this
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
    ).asserting(_ shouldBe Seq("Type mismatch." at "Box[two](str)"))
  }

  it should "accept type-level function calls that are not Type types" in {
    runForErrors(
      "def g(x: String): String = x\ndata Box[X: String](value: String)\ndef f[G: String](value: String): Box[g(G)] = Box[G](value)",
      typeArgs = Seq(GroundValue.Direct("STR", stringType))
    ).asserting(_ shouldBe Seq.empty)
  }

  // --- Transparent type aliases ---

  "a transparent alias" should "be definitionally equal to its body" in {
    runForErrors("type Id[A] = A\ndef s: String\ndef f: Id[String] = s")
      .asserting(_ shouldBe Seq.empty)
  }

  it should "merge distinct type arguments of an alias that ignores them" in {
    // `W[1]` and `W[2]` both unfold to `String`, so `W[1]` is accepted where `W[2]` is expected.
    runForErrors("type W[N: BigInteger] = String\ndef f(x: W[1]): W[2] = x")
      .asserting(_ shouldBe Seq.empty)
  }

  // --- Recursion (termination M1: rejected, not unrolled) ---

  "recursion" should "be rejected for a directly self-recursive value" in {
    runForErrors("def f: Function[BigInteger, BigInteger] = f")
      .asserting(_ shouldBe Seq("Value 'f' is defined recursively." at "f"))
  }

  it should "be rejected for mutually-recursive values" in {
    runForErrors("def f: Function[BigInteger, BigInteger] = g\ndef g: Function[BigInteger, BigInteger] = f")
      .asserting(_ shouldBe Seq("Value 'f' is defined recursively." at "f"))
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
    runW1("import eliot.jvm.IO\ndef store(x: Int): IO[Unit]\ndef main(b: Int[0, 255]): IO[Unit] = store(b)")
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
    runW1("import eliot.jvm.IO\ntype Bool\ndef bad(x: IO): Bool\ndef main(thing: IO[Unit]): Bool = bad(thing)")
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
    // `loop`'s bare `Int` return is calculated from its body, but the body re-references `loop`. The no-recursion rule
    // (termination M1) now catches this value cycle before monomorphization, so it is reported there rather than at the
    // calculated-return back-edge's fact-cache dead-lock guard (which remains a deeper defensive backstop).
    runInt(
      "def loop(x: Int): Int = loop(x)",
      name = "loop",
      typeArgs = Seq(GroundValue.Direct(BigInt(0), intType), GroundValue.Direct(BigInt(255), intType))
    ).asserting(_.map(_.message) shouldBe Seq("Value 'loop' is defined recursively."))
  }

  it should "report a value-dependent (non-stabilising) calculated return" in {
    // The growth flavour: each recursive call widens the bound (`x + x`). `grow` still refers back to itself, so the
    // no-recursion rule rejects it (the calculated-return chain never even starts).
    runInt(
      "def grow(x: Int): Int = grow(x + x)",
      name = "grow",
      typeArgs = Seq(GroundValue.Direct(BigInt(0), intType), GroundValue.Direct(BigInt(255), intType))
    ).asserting(_.map(_.message) shouldBe Seq("Value 'grow' is defined recursively."))
  }

  it should "report a mutually-recursive calculated return" in {
    // Mutual flavour: `f`'s return needs `g`'s, whose return needs `f`'s — a mutual cycle the no-recursion rule rejects.
    runInt(
      "def f(x: Int): Int = g(x)\ndef g(x: Int): Int = f(x)",
      name = "f",
      typeArgs = Seq(GroundValue.Direct(BigInt(0), intType), GroundValue.Direct(BigInt(255), intType))
    ).asserting(_.map(_.message) should contain("Value 'f' is defined recursively."))
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
    // `loopv` referencing itself is a value cycle the no-recursion rule (termination M1) rejects, before the read-site
    // calculated-return resolution is even reached.
    runInt("def loopv: Int = loopv", name = "loopv")
      .asserting(_.map(_.message) shouldBe Seq("Value 'loopv' is defined recursively."))
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

  private val boolGround: GroundValue =
    GroundValue.Structure(WellKnownTypes.boolFQN, Seq.empty, GroundValue.Type)

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
  private val matchImports: Seq[SystemImport] =
    ambientStubsWith("PatternMatch" -> ProcessorTest.patternMatchAbilityStub, "TypeMatch" -> ProcessorTest.typeMatchAbilityStub)

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
  private val w1Imports: Seq[SystemImport] = ambientStubsWith("IO" -> "type IO[A]")

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
  private val intImports: Seq[SystemImport] = ambientStubsWith(
    "BigInteger" ->
      "import eliot.lang.Bool\nimport eliot.lang.Compare\nimport eliot.lang.Arithmetic\ntype BigInteger\ndef multiplyMin(a: BigInteger, b: BigInteger, c: BigInteger, d: BigInteger): BigInteger = min(min(multiply(a, c), multiply(a, d)), min(multiply(b, c), multiply(b, d)))\ndef multiplyMax(a: BigInteger, b: BigInteger, c: BigInteger, d: BigInteger): BigInteger = max(max(multiply(a, c), multiply(a, d)), max(multiply(b, c), multiply(b, d)))",
    "Compare"    -> ProcessorTest.compareStubContent,
    "Arithmetic"    -> ProcessorTest.arithmeticStubContent,
    "Bool"       ->
      "type Bool\ndef true: Bool\ndef false: Bool\ninfix def &&(a: Bool, b: Bool): Bool\ndef fold[A](condition: Bool, whenTrue: A, whenFalse: A): A",
    "Option"     -> "type Option[A]\ndef some[A](value: A): Option[A]\ndef none[A]: Option[A]",
    "Int"        ->
      """import eliot.lang.Bool
        |import eliot.lang.Compare
        |import eliot.lang.Arithmetic
        |import eliot.compiler.Coerce
        |import eliot.compiler.Combine
        |import eliot.lang.Option
        |type Int[auto MIN: BigInteger, auto MAX: BigInteger]
        |type Byte = Int[-128, 127]
        |def nativeWiden[Smin: BigInteger, Smax: BigInteger, Tmin: BigInteger, Tmax: BigInteger](value: Int[Smin, Smax]): Int[Tmin, Tmax]
        |implement[Smin: BigInteger, Smax: BigInteger, Tmin: BigInteger, Tmax: BigInteger] Coerce[Int[Smin, Smax], Int[Tmin, Tmax]] where lessThanOrEqual(Tmin, Smin) && lessThanOrEqual(Smax, Tmax) { def coerce(value: Int[Smin, Smax]): Int[Tmin, Tmax] = nativeWiden(value) }
        |implement[Amin, Amax, Bmin, Bmax] Combine[Int[Amin, Amax], Int[Bmin, Bmax]] { type Combined = Int[min(Amin, Bmin), max(Amax, Bmax)] }
        |infix left
        |def +[LMin: BigInteger, LMax: BigInteger, RMin: BigInteger, RMax: BigInteger](left: Int[LMin, LMax], right: Int[RMin, RMax]): Int[add(LMin, RMin), add(LMax, RMax)]
        |infix left at +
        |def -[LMin: BigInteger, LMax: BigInteger, RMin: BigInteger, RMax: BigInteger](left: Int[LMin, LMax], right: Int[RMin, RMax]): Int[subtract(LMin, RMax), subtract(LMax, RMin)]
        |infix left above +
        |def *[LMin: BigInteger, LMax: BigInteger, RMin: BigInteger, RMax: BigInteger](left: Int[LMin, LMax], right: Int[RMin, RMax]): Int[multiplyMin(LMin, LMax, RMin, RMax), multiplyMax(LMin, LMax, RMin, RMax)]
        |""".stripMargin
  ) ++ Seq(
    // `Coerce`/`Combine` are the compiler-coordinated abilities the checker resolves by FQN; they live in
    // `eliot.compiler` (not the `eliot.lang` prelude), so register them there — mirroring the real layout.
    SystemImport("Coerce", "ability Coerce[From, To] { def coerce(value: From): To }", ProcessorTest.compilerPackage),
    SystemImport("Combine", "ability Combine[A, B] { type Combined }", ProcessorTest.compilerPackage)
  )

  /** Imports for the snippet that reference `coerce`/`Option` by name (`Int` itself is ambient). */
  private val intPrelude: String =
    "import eliot.compiler.Coerce\nimport eliot.lang.Option\n"

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

  // --- Effectful signatures: compile-time `Throw[String]` guard discharge (W2b) ---

  // The compile-time `Throw[String]` carrier the discharge reads back (`Right(t)` ⤳ the type `t`, `Left(msg)` ⤳ a
  // rejection). In a real build this is the always-linked compiler-platform `Either` layer; the suite supplies it as a
  // concrete `data Either` so the constructors carry the well-known `eliot.lang.Either.Left`/`Right` FQNs.
  // Override the ambient `Either` stub (a bare `type Either[E, A]`) with the concrete `data` the guard tests need,
  // rather than appending a second `Either` entry (which would register `eliot/lang/Either.els` twice).
  private val guardImports: Seq[SystemImport] =
    intImports.map(s =>
      if (s.module == "Either") s.copy(content = "data Either[E, A] = Left(error: E) | Right(value: A)") else s
    )

  // A guarded `head`: its return type is a `{Throw[String]} Type` computation that yields the payload `String` when the
  // boolean type parameter `COND` is `true` and rejects with "empty" when it is `false`. The condition is a plain `Bool`
  // type parameter (no literal-in-bounds arithmetic), keeping the test on the discharge rather than on `Int` widening.
  private val guardedHead: String =
    "def head[COND: Bool]: fold(COND, Right(String[]), Left(\"empty\")) = bar\ndef bar: String\n"

  private val trueArg  = GroundValue.Direct(true, boolGround)
  private val falseArg = GroundValue.Direct(false, boolGround)

  private def runGuard(source: String, name: String = "foo", typeArgs: Seq[GroundValue] = Seq.empty): IO[Seq[TestError]] =
    runGenerator(
      "import eliot.lang.Either\nimport eliot.lang.Bool\n" + source,
      MonomorphicValue.Key(ValueFQN(testModuleName, default(name)), typeArgs),
      guardImports
    ).map(result => toTestErrors(result._1))

  private def guardSignature(source: String, name: String, typeArgs: Seq[GroundValue]): IO[Option[GroundValue]] =
    runGenerator(
      "import eliot.lang.Either\nimport eliot.lang.Bool\n" + source,
      MonomorphicValue.Key(ValueFQN(testModuleName, default(name)), typeArgs),
      guardImports
    ).map(_._2.values.collectFirst { case v: MonomorphicValue if v.vfqn.name.name == name => v.signature })

  "effectful-signatures discharge (W2b)" should "type a `Right(t)` guard return as its payload type `t`" in {
    runGuard(guardedHead, name = "head", typeArgs = Seq(trueArg)).asserting(_ shouldBe Seq.empty)
  }

  it should "publish the discharged payload type as the monomorphic signature (the `Either` never reaches codegen)" in {
    guardSignature(guardedHead, name = "head", typeArgs = Seq(trueArg)).asserting(_ shouldBe Some(stringType))
  }

  it should "reject a `Left(msg)` guard return with the author message as the primary diagnostic" in {
    runGuard(guardedHead, name = "head", typeArgs = Seq(falseArg)).asserting(_ shouldBe Seq("empty" at "head"))
  }

  it should "discharge a bare `Right(t)` return (no `fold`) to its payload type" in {
    runGuard("def bar: String\ndef foo: Right(String[]) = bar").asserting(_ shouldBe Seq.empty)
  }

  it should "reject a bare `Left(msg)` return, even on a body-less declaration" in {
    runGuard("def foo: Left(\"boom\")").asserting(_ shouldBe Seq("boom" at "foo"))
  }

  it should "type a guarded value at a caller that observes a `Right`" in {
    runGuard(guardedHead + "def use: String = head[true]", name = "use").asserting(_ shouldBe Seq.empty)
  }

  it should "abort at a caller that observes a `Left`" in {
    runGuard(guardedHead + "def use: String = head[false]", name = "use").asserting(_ shouldBe Seq("empty" at "head"))
  }

  it should "resolve a guard through a generic intermediate at a concrete `Right` use" in {
    runGuard(guardedHead + "def wrap[COND: Bool]: String = head[COND]", name = "wrap", typeArgs = Seq(trueArg))
      .asserting(_ shouldBe Seq.empty)
  }

  it should "reject through a generic intermediate at a concrete `Left` use" in {
    runGuard(guardedHead + "def wrap[COND: Bool]: String = head[COND]", name = "wrap", typeArgs = Seq(falseArg))
      .asserting(_ shouldBe Seq("empty" at "head"))
  }

  it should "defer a guard stuck on an abstract bound to the body (no error)" in {
    // `head` monomorphized with no type argument leaves `COND` abstract, so the guard cannot reduce to `Right`/`Left`.
    // Use-Site Verification: the stuck guard is deferred to the body (its return becomes a meta the body solves) rather
    // than erroring at this abstract site; the guard is still decided at every concrete instance above.
    runGuard(guardedHead, name = "head", typeArgs = Seq.empty).asserting(_ shouldBe Seq.empty)
  }

  // --- Effectful signatures G1: guards via the combinator vocabulary (no auto-lift) ---

  // The G1 claim (docs/effectful-signatures.md): a guard written with ordinary combinators on the carrier needs *no*
  // auto-lift and *no* carrier pinning. The return is a plain function application the NbE evaluator *reduces* through
  // the combinator's body to `Right(t)`/`Left(msg)`, which the W2b discharge then reads. So this exercises the discharge
  // through real vocabulary (combinator *calls* in the signature, reduced via their bodies), not the raw inline
  // `Right`/`Left` the leaf tests above use — with zero new compiler code.
  //
  // These combinators bottom out in only the `Bool` `fold` leaf + the `Either` carrier (no `Option`/`match`): the
  // designed `Option`-mediated `when`/`orError` additionally need a compile-time `Option` carrier (a CP4-style
  // promotion — its auto-generated `match` ability must reduce at check time), which is a separate G1 sub-task.
  private val combinators: String =
    "def fail[A](msg: String): Either[String, A] = Left(msg)\n" +
      "def requireOr[A](cond: Bool, a: A, msg: String): Either[String, A] = fold(cond, Right(a), Left(msg))\n"

  // A guard in application form (no parser change): a satisfied (`COND`) `head` types as its element, an unsatisfied
  // one rejects with the author message — the whole point of length-indexed `head` on a possibly-empty collection.
  private val combinatorHead: String =
    combinators + "def head[COND: Bool]: requireOr(COND, String[], \"empty\") = bar\ndef bar: String\n"

  private def runCombinator(source: String, name: String, typeArgs: Seq[GroundValue]): IO[Seq[TestError]] =
    runGenerator(
      "import eliot.lang.Either\nimport eliot.lang.Bool\n" + source,
      MonomorphicValue.Key(ValueFQN(testModuleName, default(name)), typeArgs),
      guardImports
    ).map(result => toTestErrors(result._1))

  "effectful-signatures G1 combinators" should "type a satisfied combinator guard as its payload type" in {
    runCombinator(combinatorHead, name = "head", typeArgs = Seq(trueArg)).asserting(_ shouldBe Seq.empty)
  }

  it should "reject an unsatisfied combinator guard with the author message" in {
    runCombinator(combinatorHead, name = "head", typeArgs = Seq(falseArg)).asserting(_ shouldBe Seq("empty" at "head"))
  }

  it should "reject a bare `fail(msg)` guard with the author message" in {
    runCombinator(combinators + "def foo: fail(\"boom\")", name = "foo", typeArgs = Seq.empty)
      .asserting(_ shouldBe Seq("boom" at "foo"))
  }

  // --- The checker-side effect lift (docs/effect-lift-in-checker.md, Step 4) ---

  // The former effect-phase rewrite-shape assertions, as *monomorphic body* assertions: the auto-lift is type-directed
  // elaboration in the checker now, so the sequenced `Effect.flatMap`/`map`/`pure` shape materialises in the
  // monomorphic output (here at the stub `IO` carrier, resolved against the stub instances in `effectLiftImports` —
  // the real-impl behaviour is pinned end-to-end in the jvm `ExamplesIntegrationTest`).

  "the checker-side effect lift" should "sequence a direct-style printLine(readLine) with Effect.flatMap" in {
    liftedBody("import eliot.effect.Console\ndef echo: {Console} Unit = printLine(readLine)")
      .asserting(_ should contain("flatMap"))
  }

  it should "leave already-monadic flatMap code unchanged (no double bind)" in {
    liftedBody(
      "import eliot.effect.Console\nimport eliot.carrier.Effect\ndef echo: {Console} Unit = flatMap(s -> printLine(s), readLine)"
    ).asserting(_.count(_ == "flatMap") shouldBe 1)
  }

  it should "pass an effectful eliminator branch through unsequenced (emergent from the flex-slot deferral)" in {
    liftedBody(
      "import eliot.effect.Console\ndef echo: {Console} String = choose(readLine, readLine)\ndef choose[A](x: A, y: A): A = x"
    ).asserting(_.filter(Set("flatMap", "map", "pure")) shouldBe Seq.empty)
  }

  it should "bind the carried result of an effectful `val`, so the body sees the plain value" in {
    liftedBody(
      "import eliot.effect.Console\ndef echo: {Console} Unit = {\n  val line = readLine\n  printLine(line)\n}"
    ).asserting(_ should contain("flatMap"))
  }

  it should "thread effects through a block, selecting map for the pure tail statement" in {
    liftedBody(
      "import eliot.effect.State\ndef echo(next: String): {State[String]} String = {\n  val old = state\n  putState(next)\n  old\n}"
    ).asserting(_.filter(Set("flatMap", "map")).sorted shouldBe Seq("flatMap", "map"))
  }

  it should "bind an effectful subject dotted into a function-typed parameter (the dot-inline regression)" in {
    // `readLine.f` — `.`'s flex `a: A` slot defers, `f` rigidifies it to `String`, Phase B bind-lifts (map, pure core),
    // and the carrier-headed result then bind-lifts again into printLine's `String` slot (flatMap).
    liftedBody(
      "import eliot.effect.Console\ndef call(f: Function[String, String]): {Console} Unit = printLine(readLine.f)",
      name = "call"
    ).asserting(_.filter(Set("flatMap", "map")).sorted shouldBe Seq("flatMap", "map"))
  }

  it should "wrap a pure body under a carrier return with Effect.pure" in {
    liftedBody("import eliot.effect.Console\ndef echo: {Console} String = \"quiet\"")
      .asserting(_ should contain("pure"))
  }

  // --- The extended regression matrix (Step 5) ---

  it should "lift a deferred flex slot once a later argument rigidifies it (deferral order)" in {
    liftedBody(
      "import eliot.effect.Console\ndef pick[A](x: A, y: A): A = x\ndef echo: {Console} String = pick(readLine, \"x\")"
    ).asserting(_.filter(Set("flatMap", "map")) shouldBe Seq("map"))
  }

  it should "pass an effectful eliminator branch through while its slot stays flex (the emergent branch rule)" in {
    liftedBody(
      "import eliot.effect.Console\ndef foldOr[A, B](o: Option[A], ifNone: B, ifSome: Function[A, B]): B = ifNone\ndef echo: {Console} String = foldOr(none, readLine, s -> s)"
    ).asserting(_.filter(Set("flatMap", "map", "pure")) shouldBe Seq.empty)
  }

  it should "bind nested effectful arguments innermost-first (bind of a bind)" in {
    liftedBody(
      "import eliot.effect.Console\ndef url(s: String): String = s\ndef echo: {Console} Unit = printLine(url(readLine))"
    ).asserting(_.filter(Set("flatMap", "map")).sorted shouldBe Seq("flatMap", "map"))
  }

  it should "pass a still-flex deferred slot through and lift at the parent instead" in {
    liftedBody(
      "import eliot.effect.Console\ndef identity[A](a: A): A = a\ndef echo: {Console} Unit = printLine(identity(readLine))"
    ).asserting(_.filter(Set("flatMap", "map")) shouldBe Seq("flatMap"))
  }

  it should "bind-lift a still-flex deferred slot whose non-transparent callee cannot carry the effect up" in {
    // The counterpart of the `identity` case above. `putState[S, F](s: S): F[Unit]`'s `S` domain is a flex meta, so
    // `putState(keep(state))` defers to Phase B still flex — but unlike `identity`, `S` is absent from the `F[Unit]`
    // result, so adopting the read's carrier into `S` would strand it (never grounded → "contains unresolved
    // variable"). The fix bind-lifts here instead, threading the read: `map` sequences `keep(state)`, `flatMap`
    // sequences `putState`. This is what lets `updateState(f) = putState(f(state))` type-check.
    liftedBody(
      "import eliot.effect.State\ndef keep(s: String): String = s\ndef upd: {State[String]} Unit = putState(keep(state))",
      name = "upd"
    ).asserting(_.filter(Set("flatMap", "map")).sorted shouldBe Seq("flatMap", "map"))
  }

  it should "leave a carrier-typed storage slot unbound (the discharge-helper shape)" in {
    liftedBody(
      "type Carrier[G[_], A]\ndef discharge[G[_], A](p: Carrier[G, A]): G[A]\ndef run[G[_], A](p: Carrier[G, A]): G[A] = discharge(p)",
      name = "run",
      typeArgs = Seq(ioCarrier, stringType)
    ).asserting(_.filter(Set("flatMap", "map", "pure")) shouldBe Seq.empty)
  }

  it should "store an annotated carrier-typed let binder instead of binding it" in {
    liftedBody(
      "import eliot.jvm.IO\nimport eliot.effect.Console\nimport eliot.carrier.Effect\ndef echo: {Console} Unit = {\n  val stored: IO[String] = readLine\n  flatMap(s -> printLine(s), stored)\n}"
    ).asserting(_.filter(Set("flatMap", "map", "pure")) shouldBe Seq("flatMap"))
  }

  // Fail-safes: a non-carrier constructor never lifts; the friendly residual-check diagnostics stay; a return boundary
  // never strips a carrier. (The `Inf` subset rejection is pinned end-to-end in the jvm `TerminationIntegrationTest`.)

  it should "reject a non-carrier constructor argument with a plain mismatch (no lift)" in {
    // The mismatch anchors at the argument's application node, spanning the whole `box("x")` construction.
    liftedErrors(
      "import eliot.effect.Console\ntype Box[A]\ndef box[A](value: A): Box[A]\ndef echo: {Console} Unit = printLine(box(\"x\"))"
    ).asserting(_ should contain("Type mismatch." at "box(\"x\")"))
  }

  it should "keep the friendly declared-pure diagnostic for an effectful body under a pure return" in {
    // `echo` has no carrier binder (a pure `String` return), so it monomorphizes at no type arguments — the
    // declared-pure fail-safe now runs in the checker (its ambient-carrier-less branch), not the pre-mono phase.
    liftedErrors("import eliot.effect.Console\ndef echo: String = printLine(readLine)", typeArgs = Seq.empty)
      .asserting(
        _ should contain(
          "This value performs an effect but is declared pure; declare an effect set with { ... } or return an effect carrier." at "echo"
        )
      )
  }

  it should "reject an effectful lambda body under a rigid pure codomain (no strip at return boundaries)" in {
    // The mismatch anchors at the body's application node, spanning the whole `printLine(s)` construction.
    liftedErrors(
      "import eliot.effect.Console\ndef twice(f: Function[String, String]): String = f(f(\"x\"))\ndef echo: {Console} String = twice(s -> printLine(s))"
    ).asserting(_ should contain("Type mismatch." at "printLine(s)"))
  }

  private val ioFQN            =
    ValueFQN(ModuleName(Seq("eliot", "jvm"), "IO"), QualifiedName("IO", Qualifier.Type))
  private val ioCarrier: GroundValue = GroundValue.Structure(ioFQN, Seq.empty, GroundValue.Type)

  // The `Effect`/`Console` stubs carry trivial `implement … [IO]` instances (bodies delegating to an abstract
  // helper): the values are checked *at the ground stub `IO` carrier*, and a ground ability demand with no applicable
  // instance is a use-site error by design (the ability machinery records `NoImplementation` and the checker's
  // `AbilityResolver` reports the failed demand at the reference). The instances keep the demands resolvable; the
  // emitted refs carry the same local names (`flatMap`/`pure`/`printLine`), so the shape assertions read unchanged.
  private val effectLiftImports: Seq[SystemImport] = ambientStubsWith(
    "IO"       -> "type IO[A]",
    "Option"   -> "type Option[A]\ndef some[A](value: A): Option[A]\ndef none[A]: Option[A]",
    // The ambient stub plus the real `.` operator (mirroring `stdlib/.../Function.els`), for the dotted-subject case.
    "Function" ->
      "type Function[A, B]\ndef apply[A, B](f: Function[A, B], a: A): B\ninfix left below apply def .[A, B](a: A, f: Function[A, B]): B = f(a)",
    "Console"  ->
      ("import eliot.jvm.IO\nability Console[F[_]] {\ndef printLine(s: String): F[Unit]\ndef readLine: F[String]\n}\n" +
        "def stubConsoleIO[A]: IO[A]\n" +
        "implement Console[IO] {\ndef printLine(s: String): IO[Unit] = stubConsoleIO\ndef readLine: IO[String] = stubConsoleIO\n}"),
    // Overrides the canonical ambient `State` stub (same module, richer content) — appending a second `State`
    // SystemImport would double-register the module path.
    "State"    ->
      ("ability State[S, F[_]] {\ndef state: F[S]\ndef putState(s: S): F[Unit]\n}\n" +
        "def stubStateIO[A]: IO[A]\n" +
        "implement[S] State[S, IO] {\ndef state: IO[S] = stubStateIO\ndef putState(s: S): IO[Unit] = stubStateIO\n}")
  ) ++ Seq(
    SystemImport(
      "Effect",
      "ability Effect[F[_]] {\ndef flatMap[A, B](f: Function[A, F[B]], fa: F[A]): F[B]\ndef pure[A](a: A): F[A]\ndef map[A, B](f: Function[A, B], fa: F[A]): F[B]\n}\n" +
        "def stubEffectIO[A]: IO[A]\n" +
        "implement Effect[IO] {\ndef flatMap[A, B](f: Function[A, IO[B]], fa: IO[A]): IO[B] = stubEffectIO\ndef pure[A](a: A): IO[A] = stubEffectIO\ndef map[A, B](f: Function[A, B], fa: IO[A]): IO[B] = stubEffectIO\n}",
      ModuleName.carrierPackage
    )
  )

  /** The names of every value referenced in the named value's monomorphic body, checked at the stub `IO` carrier. */
  private def liftedBody(
      source: String,
      name: String = "echo",
      typeArgs: Seq[GroundValue] = Seq(ioCarrier)
  ): IO[Seq[String]] =
    runGenerator(
      source,
      MonomorphicValue.Key(ValueFQN(testModuleName, default(name)), typeArgs),
      effectLiftImports
    ).map(
      _._2.values
        .collectFirst { case mv: MonomorphicValue if mv.vfqn.name.name == name => mv }
        .flatMap(_.runtime)
        .map(body => referencedNames(body.value))
        .getOrElse(Seq.empty)
    )

  /** The build errors of checking the named value at the stub `IO` carrier. */
  private def liftedErrors(
      source: String,
      name: String = "echo",
      typeArgs: Seq[GroundValue] = Seq(ioCarrier)
  ): IO[Seq[TestError]] =
    runGenerator(
      source,
      MonomorphicValue.Key(ValueFQN(testModuleName, default(name)), typeArgs),
      effectLiftImports
    ).map(result => toTestErrors(result._1))

  private def referencedNames(expr: MonomorphicExpression.Expression): Seq[String] = expr match {
    case MonomorphicExpression.MonomorphicValueReference(fqn, _) => Seq(fqn.value.name.name)
    case MonomorphicExpression.FunctionApplication(target, arg)  =>
      referencedNames(target.value.expression) ++ referencedNames(arg.value.expression)
    case MonomorphicExpression.FunctionLiteral(_, _, body)       => referencedNames(body.value.expression)
    case _                                                       => Seq.empty
  }
}
