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

  it should "join divergent Int ranges across match arms (refinement channel Step 2b)" in {
    // `First -> 5` (Int[5, 5]) and `Second -> 15` (Int[15, 15]) meet at the runtime-chosen join Int[5, 15], widened
    // to the declared Int[0, 20]. Beyond the runtime result, this exercises the shadow-mode agreement harness at a
    // branch merge: the refinement channel recomputes the merge with `Meta.join` and hard-errors if it disagrees with
    // the type-level `Combine` join, so a passing compile is also a passing join check.
    compileAndRun(
      """import eliot.effect.Console
        |data Choice = First | Second
        |
        |def choose(c: Choice): Int[0, 20] = c match {
        |  case First  -> 5
        |  case Second -> 15
        |}
        |
        |def main: IO[Unit] = printLine(intToString(choose(Second)))""".stripMargin
    ).asserting(_ shouldBe "15")
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
        |import eliot.lang.Arithmetic
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
        |import eliot.lang.Arithmetic
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
        |import eliot.lang.Arithmetic
        |def product: Int[0, 1000000] = 1000 * 1000
        |
        |def main: IO[Unit] = printLine(intToString(product))""".stripMargin
    ).asserting(_ shouldBe "1000000")
  }

  // Width-agnostic arithmetic leaves (bounds-as-refinements Step 1): the jvm `+`/`-`/`*` are now three width-agnostic
  // leaves (`nativeAdd`/`nativeSubtract`/`nativeMultiply`) whose emission reads each site's operand/result
  // representations to pick the machine instruction — no `IntArith` guarded family, no per-width leaves. One program
  // doing arithmetic at four operand widths compiles to correctly-sized code for each in a single compilation, end to
  // end. (Big-operand arithmetic rides the JvmBigInteger representation, whose codegen is a separate pre-existing gap.)
  "the jvm arithmetic leaves" should "compute the right result at each operand width in one program" in {
    compileAndRun(
      """import eliot.effect.Console
        |import eliot.lang.Arithmetic
        |def byteSum: Int[0, 200] = 100 + 100
        |def shortDiff: Int[-1000, 1000] = 500 - 300
        |def intProduct: Int[0, 1000000] = 1000 * 1000
        |def longSum: Int[0, 10000000000] = 5000000000 + 5000000000
        |
        |def main: IO[Unit] = {
        |   printLine(intToString(byteSum))
        |   printLine(intToString(shortDiff))
        |   printLine(intToString(intProduct))
        |   printLine(intToString(longSum))
        |}""".stripMargin
    ).asserting(_ shouldBe "200\n200\n1000000\n10000000000")
  }

  // The headline benefit of the guarded family over the old ordered `fold` chain: a *coverage gap* in the range guards
  // is a hard error at the manifest instantiation, not a silent wrong-width leaf. A user `Widen` ability covering only
  // the byte range, used at an int-width range, finds no surviving instance — "No ability implementation found" at the
  // concrete `[0, 100000]` — rather than mis-selecting.
  "a guarded ability family with a coverage gap" should "hard-error at the uncovered instantiation" in {
    compileForErrors(
      """import eliot.effect.Console
        |
        |ability Widen[Mn: BigInteger, Mx: BigInteger] {
        |   def widen(x: Int[Mn, Mx]): Int[Mn, Mx]
        |}
        |
        |implement[Mn: BigInteger, Mx: BigInteger] Widen[Mn, Mx] where fitsIn[-128, 127, Mn, Mx] {
        |   def widen(x: Int[Mn, Mx]): Int[Mn, Mx] = x
        |}
        |
        |def useBig(x: Int[0, 100000]): Int[0, 100000] = widen(x)
        |
        |def main: IO[Unit] = printLine(intToString(useBig(50000)))""".stripMargin
    ).asserting(_ should include("No ability implementation found for ability 'Widen'"))
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
        |import eliot.lang.Arithmetic
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
        |import eliot.lang.Arithmetic
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
        |import eliot.lang.Arithmetic
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

  // --- Arithmetic ability: heterogeneous add/subtract/multiply on bounded Int, with exact (non-widened) result bounds. ---

  // Nested calls to the same ability method must resolve independently: `add(a, add(b, c))` widens the range in two
  // steps to Int[0, 175]. Before per-reference associated-type meta identity, the outer and inner `add` shared one
  // AddResult meta and the outer inherited the inner's bounds; the exact Int[0, 175] annotation would then fail to
  // type-check. The exact annotation compiling (and the value being correct) proves both.
  "the Arithmetic ability" should "add ranged ints with an exact widened result bound across nested calls" in {
    compileAndRun(
      """import eliot.effect.Console
        |import eliot.lang.Arithmetic
        |
        |def a: Int[0, 100] = 30
        |def b: Int[0, 50] = 20
        |def c: Int[0, 25] = 10
        |
        |def total: Int[0, 175] = add(a, add(b, c))
        |
        |def main: IO[Unit] = printLine(intToString(total))""".stripMargin
    ).asserting(_ shouldBe "60")
  }

  // A declared result bound too tight for the computed sum is rejected — evidence the bound is genuinely computed
  // (Int[0, 100] + Int[0, 50] = Int[0, 150], which does not fit the declared Int[0, 100]).
  it should "reject a declared result bound too tight for the computed sum" in {
    compileForErrors(
      """import eliot.effect.Console
        |import eliot.lang.Arithmetic
        |
        |def a: Int[0, 100] = 30
        |def b: Int[0, 50] = 20
        |
        |def tooTight: Int[0, 100] = add(a, b)
        |
        |def main: IO[Unit] = printLine(intToString(tooTight))""".stripMargin
    ).asserting(_ should include("mismatch"))
  }

  // Constraint-based associated-type projection: a plain generic function whose return is the bare associated type
  // `AddResult` of its `~ Arithmetic[X, Y]` constraint. The concrete return (Int[0, 150] here) is projected at the call
  // from the monomorphized callee via the calculated-return back-edge. The exact Int[0, 150] annotation confirms it.
  it should "project the associated result type of a generic function's constraint" in {
    compileAndRun(
      """import eliot.effect.Console
        |import eliot.lang.Arithmetic
        |
        |def plus[X, Y ~ Arithmetic[X, Y]](x: X, y: Y): AddResult = add(x, y)
        |
        |def a: Int[0, 100] = 30
        |def b: Int[0, 50] = 20
        |
        |def total: Int[0, 150] = plus(a, b)
        |
        |def main: IO[Unit] = printLine(intToString(total))""".stripMargin
    ).asserting(_ shouldBe "50")
  }

  // Generic arithmetic composed over all three result types, with a generic call feeding another. `times(a, b)` is
  // Int[0, 5000] (value 600), and `plus(that, a)` projects AddResult again (value 630).
  it should "compose generic arithmetic over all three result types" in {
    compileAndRun(
      """import eliot.effect.Console
        |import eliot.lang.Arithmetic
        |
        |def plus[X, Y ~ Arithmetic[X, Y]](x: X, y: Y): AddResult = add(x, y)
        |def times[X, Y ~ Arithmetic[X, Y]](x: X, y: Y): MulResult = multiply(x, y)
        |
        |def a: Int[0, 100] = 30
        |def b: Int[0, 50] = 20
        |
        |def main: IO[Unit] = printLine(intToString(plus(times(a, b), a)))""".stripMargin
    ).asserting(_ shouldBe "630")
  }

  // --- Associated types as first-class type functions: `AddResult[X, Y]` applied directly in a signature. ---

  // A direct application of an ability's associated type to ground arguments reduces through the matching instance:
  // `AddResult[Int[0, 1], Int[2, 3]]` IS `Int[2, 4]`, so the widening literal compiles and runs.
  "associated type application" should "reduce a directly applied associated type in a signature" in {
    compileAndRun(
      """import eliot.effect.Console
        |import eliot.lang.Arithmetic
        |
        |def four: AddResult[Int[0, 1], Int[2, 3]] = 4
        |
        |def main: IO[Unit] = printLine(intToString(four))""".stripMargin
    ).asserting(_ shouldBe "4")
  }

  // The reduced application genuinely constrains the body: 5 does not fit Int[2, 4]. (This used to be silently
  // accepted — the applied associated-type return was discarded as a calculated return and refilled from the body.)
  it should "reject a body outside the reduced associated type's range" in {
    compileForErrors(
      """import eliot.effect.Console
        |import eliot.lang.Arithmetic
        |
        |def five: AddResult[Int[0, 1], Int[2, 3]] = 5
        |
        |def main: IO[Unit] = printLine(intToString(five))""".stripMargin
    ).asserting(_ should include("mismatch"))
  }

  // --- Interval: the split-instance client of associated-type applications (endpoint-wise interval arithmetic). ---

  // Interval addition is endpoint-wise; the exact endpoint annotations compiling proves the instance's result formula
  // `Interval[AddResult[S1, S2], AddResult[E1, E2]]` reduced to the exact endpoint types.
  "the Interval type" should "add intervals with exact endpoint result types" in {
    compileAndRun(
      """import eliot.effect.Console
        |import eliot.lang.Arithmetic
        |import eliot.lang.Interval
        |
        |def a: Interval[Int[0, 0], Int[1, 1]] = Interval(0, 1)
        |def b: Interval[Int[1, 1], Int[2, 2]] = Interval(1, 2)
        |
        |def sum: Interval[Int[1, 1], Int[3, 3]] = a + b
        |
        |def main: IO[Unit] = {
        |  printLine(intToString(sum.start))
        |  printLine(intToString(sum.end))
        |}""".stripMargin
    ).asserting(_ shouldBe "1\n3")
  }

  // Subtraction pairs a start with the other interval's end (the crossing is value-level); multiplication selects the
  // min/max of the four corner products at runtime (via the `Compare[Int]` ordering leaf), with both result endpoints
  // in the corners' `Combine` join.
  it should "subtract and multiply intervals with exact endpoint result types" in {
    compileAndRun(
      """import eliot.effect.Console
        |import eliot.lang.Arithmetic
        |import eliot.lang.Interval
        |
        |def a: Interval[Int[0, 0], Int[1, 1]] = Interval(0, 1)
        |def b: Interval[Int[1, 1], Int[2, 2]] = Interval(1, 2)
        |
        |def diff: Interval[Int[-2, -2], Int[0, 0]] = a - b
        |def prod: Interval[Int[0, 2], Int[0, 2]] = a * b
        |
        |def main: IO[Unit] = {
        |  printLine(intToString(diff.start))
        |  printLine(intToString(diff.end))
        |  printLine(intToString(prod.start))
        |  printLine(intToString(prod.end))
        |}""".stripMargin
    ).asserting(_ shouldBe "-2\n0\n0\n2")
  }

  // A declared endpoint type the computed one does not fit is rejected — the interval result types are genuinely
  // computed, not widened or inferred from the body.
  it should "reject a declared interval endpoint the computed sum does not produce" in {
    compileForErrors(
      """import eliot.effect.Console
        |import eliot.lang.Arithmetic
        |import eliot.lang.Interval
        |
        |def a: Interval[Int[0, 0], Int[1, 1]] = Interval(0, 1)
        |def b: Interval[Int[1, 1], Int[2, 2]] = Interval(1, 2)
        |
        |def wrong: Interval[Int[0, 0], Int[3, 3]] = a + b
        |
        |def main: IO[Unit] = printLine(intToString(wrong.start))""".stripMargin
    ).asserting(_ should include("mismatch"))
  }
}
