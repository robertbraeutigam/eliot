package com.vanillasource.eliot.eliotc.jvm

/** Part 4 of 4 of the end-to-end example-program suite (see [[FullIntegrationTest]]). The 88 cases are split across
  * four classes so Mill runs them in four parallel test-worker JVMs — each warming its own resident compilation
  * session — instead of one worker compiling all 88 serially. Keep each part self-contained: a case that shares a
  * class-level helper (e.g. `orderingPrelude`) must stay in the same part as that helper. */
class ExamplesIntegrationTest4 extends FullIntegrationTest {

  "integer range widening" should "widen a bare literal into a broader declared range at runtime" in {
    compileAndRun(
      """import eliot.effect.Console
        |def widened: Int[0, 1000] = 7
        |
        |def main: IO[Unit] = printLine(intToString(widened))""".stripMargin
    ).asserting(_ shouldBe "7")
  }

  it should "accept a literal assigned through a width alias and print it" in {
    compileAndRun(
      """import eliot.effect.Console
        |def small: Byte = 42
        |
        |def main: IO[Unit] = printLine(intToString(small))""".stripMargin
    ).asserting(_ shouldBe "42")
  }

  it should "construct and access a record with a bare Int field (W2)" in {
    // `Counter`'s bare `Int` field generalizes the type to `Counter[lo, hi]`; the accessor `n` (a match under the hood)
    // recovers the field. Exercises construct + accessor + handleCases end-to-end at runtime.
    compileAndRun(
      """import eliot.effect.Console
        |data Counter(n: Int)
        |
        |def main: IO[Unit] = printLine(intToString(n(Counter(42))))""".stripMargin
    ).asserting(_ shouldBe "42")
  }

  it should "round-trip a record field through an explicit match (W2)" in {
    compileAndRun(
      """import eliot.effect.Console
        |data Counter(n: Int)
        |
        |def field(c: Counter[7, 7]): Int[7, 7] = c match { case Counter(x) -> x }
        |
        |def main: IO[Unit] = printLine(intToString(field(Counter(7))))""".stripMargin
    ).asserting(_ shouldBe "7")
  }

  // W2 follow-up: type-level matching over an auto-bounded record. `Counter`'s bare `Int` field grows the type to
  // `Counter[lo, hi]`, so the `typeMatch` matcher's handler must bind both synthesized bounds (`case Counter[lo, hi]`).
  it should "type-level match over an auto-bounded record (W2 follow-up)" in {
    compileAndRun(
      """import eliot.effect.Console
        |data Counter(n: Int)
        |
        |def describe(t: Type): String = t match {
        |   case Counter[lo, hi] -> "counter"
        |   case _               -> "<other>"
        |}
        |
        |def main: IO[Unit] = printLine(describe(Counter[0, 255]))""".stripMargin
    ).asserting(_ shouldBe "counter")
  }

  it should "widen an arithmetic result into a broader declared range at runtime" in {
    compileAndRun(
      """import eliot.effect.Console
        |def total: Int[0, 1000] = 3 + 4
        |
        |def main: IO[Unit] = printLine(intToString(total))""".stripMargin
    ).asserting(_ shouldBe "7")
  }

  // W3: a bare `Int` return is *calculated* from the body. `double`'s return bounds come from `x + x`, not from the
  // (omitted) source annotation; the caller observes them off `double`'s monomorphized signature and runs it.
  it should "calculate a bare Int return from the body and run it (W3)" in {
    compileAndRun(
      """import eliot.effect.Console
        |def double(x: Int): Int = x + x
        |
        |def main: IO[Unit] = printLine(intToString(double(21)))""".stripMargin
    ).asserting(_ shouldBe "42")
  }

  it should "calculate a bare data return from the body and run it (W3)" in {
    compileAndRun(
      """import eliot.effect.Console
        |data Counter(n: Int)
        |
        |def mk(v: Int): Counter = Counter(v)
        |
        |def main: IO[Unit] = printLine(intToString(n(mk(42))))""".stripMargin
    ).asserting(_ shouldBe "42")
  }

  // Phase 3: representation selection. Each range below lowers to a different JVM wrapper (Short/Integer/Long); a wrong
  // width would overflow and print the wrong number, so these assert the chosen representation is wide enough.
  "integer representation selection" should "carry an Int[0, 70000] value at the 32-bit representation" in {
    compileAndRun(
      """import eliot.effect.Console
        |def big: Int[0, 70000] = 70000
        |
        |def main: IO[Unit] = printLine(intToString(big))""".stripMargin
    ).asserting(_ shouldBe "70000")
  }

  it should "compute a product that overflows 16 bits at the 32-bit representation" in {
    compileAndRun(
      """import eliot.effect.Console
        |def product: Int[0, 1000000] = 1000 * 1000
        |
        |def main: IO[Unit] = printLine(intToString(product))""".stripMargin
    ).asserting(_ shouldBe "1000000")
  }

  it should "carry a value beyond 32 bits at the 64-bit representation" in {
    compileAndRun(
      """import eliot.effect.Console
        |def huge: Int[0, 5000000000] = 5000000000
        |
        |def main: IO[Unit] = printLine(intToString(huge))""".stripMargin
    ).asserting(_ shouldBe "5000000000")
  }

  // Mangling/dedup: a generic function instantiated at two ranges that share a representation (both `Byte`) lowers to a
  // single concrete signature, so the per-range mangled methods collapse correctly and both calls dispatch.
  "generic instantiation" should "reuse one method for two same-representation ranges at runtime" in {
    compileAndRun(
      """import eliot.effect.Console
        |def id[Mn: BigInteger, Mx: BigInteger](x: Int[Mn, Mx]): Int[Mn, Mx] = x
        |def a: Int[0, 3] = 3
        |def b: Int[0, 5] = 5
        |
        |def main: IO[Unit] = printLine(intToString(id(a) + id(b)))""".stripMargin
    ).asserting(_ shouldBe "8")
  }

  // The same generic at two ranges with *different* representations (`Byte` and `Long`) lowers to two distinct concrete
  // signatures — collision-free overloads — and each call dispatches to the one matching its width.
  it should "dispatch distinct methods for two different-representation ranges at runtime" in {
    compileAndRun(
      """import eliot.effect.Console
        |def id[Mn: BigInteger, Mx: BigInteger](x: Int[Mn, Mx]): Int[Mn, Mx] = x
        |def a: Int[0, 3] = 3
        |def big: Int[0, 5000000000] = 5000000000
        |
        |def main: IO[Unit] = printLine(intToString(id(a) + id(big)))""".stripMargin
    ).asserting(_ shouldBe "5000000003")
  }

  // Eliminating a multi-field `data` desugars its `match` into an N-parameter handler (`x -> y -> body`), which the
  // backend lowers into nested single-argument closures. Extracting the *first* field exercises the outer closure.
  "multi-field data" should "extract the first field of a two-field constructor" in {
    compileAndRun(
      """import eliot.effect.Console
        |data Pair(a: String, b: String)
        |
        |def first(p: Pair): String = p match {
        |  case Pair(x, y) -> x
        |}
        |
        |def main: IO[Unit] = printLine(first(Pair("hello", "world")))""".stripMargin
    ).asserting(_ shouldBe "hello")
  }

  // Extracting the *second* field returns the inner closure's parameter, so it checks the captured-variable plumbing
  // across the peeled frames.
  it should "extract the second field of a two-field constructor" in {
    compileAndRun(
      """import eliot.effect.Console
        |data Pair(a: String, b: String)
        |
        |def second(p: Pair): String = p match {
        |  case Pair(x, y) -> y
        |}
        |
        |def main: IO[Unit] = printLine(second(Pair("hello", "world")))""".stripMargin
    ).asserting(_ shouldBe "world")
  }

  // A three-field constructor peels into three nested closures; selecting the middle field confirms more than two
  // levels of nesting resolve their parameters correctly.
  it should "extract the middle field of a three-field constructor" in {
    compileAndRun(
      """import eliot.effect.Console
        |data Triple(a: String, b: String, c: String)
        |
        |def middle(t: Triple): String = t match {
        |  case Triple(x, y, z) -> y
        |}
        |
        |def main: IO[Unit] = printLine(middle(Triple("one", "two", "three")))""".stripMargin
    ).asserting(_ shouldBe "two")
  }

  // A closure that references a captured variable more than once must capture it exactly once. `freeVariables` reports
  // every occurrence (it does not de-duplicate), so without `LambdaGenerator` de-duping the capture set, the generated
  // closure class would declare the field `s` twice — a `ClassFormatError: Duplicate field name` at load time.
  it should "generate a closure that captures the same variable twice" in {
    compileAndRun(
      """import eliot.effect.Console
        |def firstOf(a: String, b: String): String = a
        |
        |def make(s: String): Function[Unit, String] = ignore -> firstOf(s, s)
        |
        |def main: IO[Unit] = printLine(apply(make("captured-twice"), unit))""".stripMargin
    ).asserting(_ shouldBe "captured-twice")
  }

  // A two-field `Int` round-trip: the fields lower to different representations (`Byte`-range and `Long`-range), so
  // matching them out and summing also exercises cross-representation arithmetic over the peeled closures.
  it should "match out two integer fields at different representations and sum them" in {
    compileAndRun(
      """import eliot.effect.Console
        |data IntPair(small: Int[0, 255], large: Int[0, 5000000000])
        |
        |def sum(p: IntPair): Int[0, 5000000255] = p match {
        |  case IntPair(s, l) -> s + l
        |}
        |
        |def main: IO[Unit] = printLine(intToString(sum(IntPair(200, 5000000000))))""".stripMargin
    ).asserting(_ shouldBe "5000000200")
  }

  // A *generic* multi-field constructor is emitted once (one class + factory per data type) but used at two
  // instantiations whose bare type-parameter fields have different JVM carriers (`Pair[Box[String], String]` stores a
  // `Box`, `Pair[String, String]` stores a `String`). Both the factory and every call site must erase the polymorphic
  // fields to `Object`, or the single shared `Pair(..)` descriptor mismatches the call site (a runtime
  // `NoSuchMethodError`). This is the generic-multi-field codegen bug that previously blocked two-field generic data.
  it should "construct and match a generic two-field constructor at two distinct instantiations" in {
    compileAndRun(
      """import eliot.effect.Console
        |data Box[A](item: A)
        |
        |data Pair[A, B](first: A, second: B)
        |
        |def secondOf[A, B](p: Pair[A, B]): B = p match {
        |  case Pair(x, y) -> y
        |}
        |
        |def main: IO[Unit] = printLine(secondOf(Pair(Box("boxed"), secondOf(Pair("c", "plain")))))""".stripMargin
    ).asserting(_ shouldBe "plain")
  }

  // The same erasure, but with a *mixed* constructor: a concrete `tag: String` field beside a polymorphic `value: A`
  // field, used at `Tagged[String]` and `Tagged[Tagged[String]]`. The concrete field must keep its `String` descriptor
  // (only the polymorphic field erases), and the two instantiations must still agree on the shared factory. Exercised
  // through the auto-generated single-constructor field accessor (`value`), i.e. the pattern-match codegen path.
  it should "construct and access a generic constructor mixing a concrete and a polymorphic field" in {
    compileAndRun(
      """import eliot.effect.Console
        |data Tagged[A](tag: String, value: A)
        |
        |def main: IO[Unit] = printLine(value(value(Tagged("outer", Tagged("inner", "deep")))))""".stripMargin
    ).asserting(_ shouldBe "deep")
  }

  // A single-constructor *union* whose constructor name differs from the type name (`data Color = Red`). The data class
  // must be named after the *type* (`Color`), not the constructor (`Red`): the factory's declared return, the match
  // parameter type, and the `PatternMatch$Color$impl` singleton all refer to `Color`, so naming the class `Red` made the
  // factory `Red()` return a `Red` while every call site expected a `Color` (a runtime `NoSuchMethodError`). Records
  // (constructor name = type name) always coincided and were never affected.
  it should "construct and match a single-constructor union whose constructor name differs from the type" in {
    compileAndRun(
      """import eliot.effect.Console
        |data Color = Red
        |
        |def name(c: Color): String = c match {
        |  case Red -> "red"
        |}
        |
        |def main: IO[Unit] = printLine(name(Red))""".stripMargin
    ).asserting(_ shouldBe "red")
  }

  // The same single-constructor-union naming, but with a field and a generic parameter, so it also rides the
  // bare-type-parameter erasure: `Box[A] = Wrap(item: A)` (constructor `Wrap` ≠ type `Box`) accessed via the
  // auto-generated `item` accessor.
  it should "construct and access a generic single-constructor union whose constructor name differs from the type" in {
    compileAndRun(
      """import eliot.effect.Console
        |data Box[A] = Wrap(item: A)
        |
        |def main: IO[Unit] = printLine(item(Wrap("wrapped")))""".stripMargin
    ).asserting(_ shouldBe "wrapped")
  }
}
