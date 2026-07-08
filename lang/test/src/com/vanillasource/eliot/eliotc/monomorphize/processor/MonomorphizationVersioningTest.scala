package com.vanillasource.eliot.eliotc.monomorphize.processor

import cats.effect.IO
import com.vanillasource.eliot.eliotc.ProcessorTest
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.monomorphize.fact.MonomorphicValue
import com.vanillasource.eliot.eliotc.plugin.LangProcessors
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}
import com.vanillasource.eliot.eliotc.stdlib.plugin.StdlibNativesProcessor
import com.vanillasource.eliot.eliotc.used.UsedNames

import scala.concurrent.duration.*

/** Characterization test suite — Deliverable 0 of the monomorphization-keying plan.
  *
  * These tests pin down how many monomorphic versions the compiler unrolls for each keying-plan scenario S1-S8: the
  * representation win (S2) collapses to one version and the *must-not-over-merge* guards (S3/S6) stay put. The four
  * recursion-shaped scenarios (S1/S5/S7/S8) were always flagged as proxies "no real program can produce" (Eliot user
  * code cannot recurse) that only unrolled because reject-recursion was unwired; that rule (termination M1) is now
  * wired, so each of those is rejected outright as recursion — the non-recursive scenarios carry the keying coverage.
  *
  * The metric is the number of distinct [[MonomorphicValue]] facts the `used` traversal materialises per `vfqn`. The
  * harness drives [[UsedNames]] from a concrete `main`, which demands a `MonomorphicValue` for every reachable
  * instantiation; counting the materialised facts grouped by `vfqn` is exactly the codegen breadth the projection
  * targets. (The plan's optional post-uncurry mirror — counting `UncurriedMonomorphicValue` — is not wired here; that
  * stage lives in the jvm backend and the breadth being deduped is already visible at the `MonomorphicValue` layer.)
  *
  * Historical note: before reject-recursion was wired, S1/S5 unrolled their reified index per step (4 and 3 versions)
  * once a read-back gap was fixed (`BindingClosure.reifyingWrap`; see `ComputedTypeArgumentReadbackTest`), and S7's
  * unbounded `Box[...]` tower was caught by the `used` non-convergence backstop. With termination M1 wired, every value
  * cycle is rejected before monomorphization, so those scenarios now assert the recursion error rather than a version
  * count; the backstop survives as a defensive fail-safe for residual type-level (Type:Type/Girard) divergence.
  */
// A small `UsedNamesProcessor` backstop tolerance keeps the divergent scenario (S7) cheap and deterministic; every
// converging scenario here nests at most 4 deep (S1's countdown), well under it, so the counts are unaffected.
// `StdlibNativesProcessor` (stdlib-layer arithmetic/`&&`/`lessThanOrEqual` natives backing `Int`'s bounds) composed
// onto `LangProcessors`, with its native label registered so the binding merger consults it.
class MonomorphizationVersioningTest
    extends ProcessorTest(
      (LangProcessors(maxNestedRepeats = 8, extraNativeBindingLabels = Seq(StdlibNativesProcessor.stdlibLabel)) :+
        StdlibNativesProcessor())*
    ) {

  // --- S1: recursion over a size-bounded structure (Cat 1, phantom index) ----------------------------------------
  // This used to be a *recursion-shaped proxy* for size-indexed unrolling: `countdown[N]` recursing to `countdown[N-1]`
  // with a `fold` base case at `N <= 0`, which once materialised four versions {3, 2, 1, 0} to pin the keying breadth.
  // It was always flagged as something "no real program can produce — Eliot user code cannot recurse" and only unrolled
  // because reject-recursion was not yet wired. That rule (termination M1) is now wired: `countdown` refers back to
  // itself, so it is rejected outright. The non-recursive keying scenarios below (S2/S3/S4/S6) still pin the dedup.
  "S1 (size-indexed recursion, reified index)" should "be rejected as recursion" in {
    runVersioning(
      "import eliot.lang.Bool\nimport eliot.lang.Order\ndef bigOf[V: BigInteger]: BigInteger = V\ndef countdown[N: BigInteger]: BigInteger = fold(lessThanOrEqual(N, bigOf[0]), bigOf[0], countdown[subtract(N, bigOf[1])])\ndef main: BigInteger = countdown[3]",
      imports = intImports
    ).asserting { case (errors, _) =>
      errors.map(_.message) should contain("Value 'countdown' is defined recursively.")
    }
  }

  // --- S2: two call sites Int[0,100] & Int[0,50] (Cat 2 representation, same width) -------------------------------
  // A single generic `id` is instantiated at two distinct bounds that share a machine representation (both fit a byte).
  // `id`'s `A` is representation-class (CollapseToRepresentation), so the codegen projection (B2/B3) keys it on the
  // head-preserving representation: `Int[0, 100]` and `Int[0, 50]` both lower to `JvmByte`, collapsing to one version.
  // This needs a representation-bearing `Int` (`reprIntImports`) — with the abstract stub `representationOf` is the
  // identity and the bounds would never collapse.
  "S2 (representation, same width)" should "collapse to one version per representation (was 2, now 1)" in {
    runVersioning(
      "def a: Int[0, 100]\ndef b: Int[0, 50]\ndef id[A](x: A): A = x\ndef use[A, B](x: A, y: B): A = x\ndef main: Int[0, 100] = use(id(a), id(b))",
      imports = reprIntImports
    ).asserting { case (errors, counts) =>
      (errors, countOf(counts, "id")) shouldBe (Seq.empty, 1)
    }
  }

  // --- S3: Int[0,100] & Int[0,100000] (Cat 2 representation, different width) -------------------------------------
  // Same shape as S2 but the second bound needs a wider representation than a byte (`JvmByte` vs `JvmInt`), so the two
  // instances are genuinely different code. The projection keys them on distinct representations, so they STAY two —
  // a must-not-over-merge guard for the rep-collapse projection. (Also uses `reprIntImports`: with the abstract stub
  // they would stay 2 only by accident of differing bounds, not differing representations.)
  "S3 (representation, different width)" should "keep one version per distinct representation (stays 2)" in {
    runVersioning(
      "def a: Int[0, 100]\ndef b: Int[0, 100000]\ndef id[A](x: A): A = x\ndef use[A, B](x: A, y: B): A = x\ndef main: Int[0, 100] = use(id(a), id(b))",
      imports = reprIntImports
    ).asserting { case (errors, counts) =>
      (errors, countOf(counts, "id")) shouldBe (Seq.empty, 2)
    }
  }

  // --- S4: reified value at K finite call sites (Cat 3, recursion-invariant) --------------------------------------
  // `tag[N]: BigInteger = N` materialises its erased `N` into a runtime constant (reification). At K=3 distinct call
  // sites each version is genuinely different code. TODAY: 3. TARGET: 3 (correct — a bounded reified family stays
  // distinct; it neither collapses nor erases).
  "S4 (reified value, recursion-invariant)" should "unroll one version per distinct reified constant (target: K, correct)" in {
    runVersioning(
      "def tag[N: BigInteger]: BigInteger = N\ndef use[A, B, C](x: A, y: B, z: C): A = x\ndef main: BigInteger = use(tag[1], tag[2], tag[3])"
    ).asserting { case (errors, counts) =>
      (errors, countOf(counts, "tag")) shouldBe (Seq.empty, 3)
    }
  }

  // --- S5: reified value varying per recursion (Cat 3, self-referential) ------------------------------------------
  // Like S1, a recursion-shaped proxy (`gen[N]` reifying its index each step) that once unrolled to {3, 2, 1} only
  // because reject-recursion was unwired. `gen` refers back to itself, so termination M1 now rejects it. The disposition
  // it once pinned (a reified binder classifies as Specialize) stays covered non-recursively by S4 (`tag`).
  "S5 (reified value, self-referential)" should "be rejected as recursion" in {
    runVersioning(
      "import eliot.lang.Bool\nimport eliot.lang.Order\ndef bigOf[V: BigInteger]: BigInteger = V\ndef gen[N: BigInteger]: BigInteger = fold(lessThanOrEqual(N, bigOf[0]), bigOf[0], add(N, gen[subtract(N, bigOf[1])]))\ndef main: BigInteger = gen[3]",
      imports = intImports
    ).asserting { case (errors, _) =>
      errors.map(_.message) should contain("Value 'gen' is defined recursively.")
    }
  }

  // --- S6: two types, same representation, different ability impl (dispatch) --------------------------------------
  // `Celsius` and `Fahrenheit` are distinct nominal types that share a representation (both single-field records) but
  // select different `Show` instances. The generic `describe` is dispatched at both. TODAY: 2. TARGET: 2 (must stay —
  // the projection must preserve the nominal head and never merge distinct dispatch by representation alone).
  "S6 (dispatch, same representation)" should "unroll one version per dispatched type (target: 2, must stay)" in {
    runVersioning(
      "ability Show[A] { def render(x: A): String }\ndata Celsius(cv: BigInteger)\ndata Fahrenheit(fv: BigInteger)\nimplement Show[Celsius] { def render(x: Celsius): String = \"c\" }\nimplement Show[Fahrenheit] { def render(x: Fahrenheit): String = \"f\" }\ndef describe[A ~ Show](x: A): String = render(x)\ndef c: Celsius\ndef f: Fahrenheit\ndef use[A, B](x: A, y: B): A = x\ndef main: String = use(describe(c), describe(f))"
    ).asserting { case (errors, counts) =>
      (errors, countOf(counts, "describe")) shouldBe (Seq.empty, 2)
    }
  }

  // --- S7: divergent recursion (no base case) --------------------------------------------------------------------
  // The `Box` tower `loop[A] -> loop[Box[A]] -> ...` once diverged the `used` traversal, then was caught by the
  // non-convergence backstop in `UsedNamesProcessor`. `loop` refers back to itself, so termination M1 now rejects the
  // value cycle *before* monomorphization — the only way to reach unbounded monomorphic breadth was a value cycle, which
  // is now banned, so the backstop survives purely as a defensive fail-safe for residual type-level (Type:Type/Girard)
  // divergence that no longer has a value-level proxy to exercise it here.
  "S7 (divergent recursion, no base case)" should "be rejected as recursion" in {
    runVersioning(
      "data Box[A](unwrap: A)\ndef loop[A](x: A): A = unwrap(loop(Box(x)))\ndef bi: BigInteger\ndef main: BigInteger = loop(bi)"
    ).timeout(30.seconds).asserting { case (errors, _) =>
      errors.map(_.message) should contain("Value 'loop' is defined recursively.")
    }
  }

  // --- S8: recursion on a runtime value, invariant type (control / regression guard) -----------------------------
  // A plain self-recursive value (`main = main`) that once unrolled to exactly one version. It refers back to itself, so
  // termination M1 now rejects it.
  "S8 (runtime recursion, invariant type)" should "be rejected as recursion" in {
    runVersioning("def main: Function[BigInteger, BigInteger] = main")
      .timeout(2.seconds)
      .asserting { case (errors, _) =>
        errors.map(_.message) should contain("Value 'main' is defined recursively.")
      }
  }

  // --- Helpers ---------------------------------------------------------------------------------------------------

  /** The number of distinct materialised [[MonomorphicValue]] specializations per original `vfqn`. */
  private def versionCounts(facts: Map[CompilerFactKey[?], CompilerFact]): Map[ValueFQN, Int] =
    facts.values.collect { case mv: MonomorphicValue => mv }.groupBy(_.vfqn).view.mapValues(_.size).toMap

  private def countOf(counts: Map[ValueFQN, Int], name: String): Int =
    counts.getOrElse(ValueFQN(testModuleName, default(name)), 0)

  /** Imports providing the full ambient `Int` environment (range bounds + `Coerce`/`Combine`/arithmetic + the `fold`
    * compile-time native), mirroring `MonomorphicTypeCheckTest.intImports`. The size-indexed / reified recursion
    * scenarios need `fold` (compile-time branch selection on the index), `lessThanOrEqual`, `subtract` and `add`.
    */
  private val intImports: Seq[SystemImport] = ambientStubsWith(
    "BigInteger" ->
      "import eliot.lang.Bool\nimport eliot.lang.Order\ntype BigInteger\ndef add(a: BigInteger, b: BigInteger): BigInteger\ndef subtract(a: BigInteger, b: BigInteger): BigInteger\ndef multiplyMin(a: BigInteger, b: BigInteger, c: BigInteger, d: BigInteger): BigInteger\ndef multiplyMax(a: BigInteger, b: BigInteger, c: BigInteger, d: BigInteger): BigInteger",
    "Order"      -> ProcessorTest.orderStubContent,
    "Bool"       ->
      "type Bool\ndef true: Bool\ndef false: Bool\ninfix def &&(a: Bool, b: Bool): Bool\ndef fold[A](condition: Bool, whenTrue: A, whenFalse: A): A",
    "Option"     -> "type Option[A]\ndef some[A](value: A): Option[A]\ndef none[A]: Option[A]",
    "Int"        ->
      """import eliot.lang.Bool
        |import eliot.lang.Order
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

  /** Like [[intImports]] but with a *representation-bearing* `Int`: the `opaque type Int[MIN, MAX]` body that folds a
    * range down to the narrowest `Jvm*` width (mirroring the real jvm-layer `Int.els`), plus the `JvmByte`/… repr types
    * and the `fitsIn` predicate the body folds on. This is what makes the representation-collapse projection (S2/S3)
    * observable: with the abstract `Int` stub `RepresentationLowering.representationOf` is the identity, so the
    * codegen projection could never tell a same-width pair (`Int[0, 100]`/`Int[0, 50]`, S2 -> 1) from a different-width
    * pair (`Int[0, 100]`/`Int[0, 100000]`, S3 -> 2).
    */
  private val reprIntImports: Seq[SystemImport] = intImports.map {
    case imp @ SystemImport("BigInteger", content, _) =>
      imp.copy(content =
        content +
          "\ndef fitsIn(lo: BigInteger, hi: BigInteger, min: BigInteger, max: BigInteger): Bool = lessThanOrEqual(lo, min) && lessThanOrEqual(max, hi)"
      )
    case imp @ SystemImport("Int", content, _)        =>
      imp.copy(content =
        content.replace(
          "type Int[auto MIN: BigInteger, auto MAX: BigInteger]\n",
          """type JvmByte
            |type JvmShort
            |type JvmInt
            |type JvmLong
            |type JvmBigInteger
            |opaque type Int[auto MIN: BigInteger, auto MAX: BigInteger] =
            |  fold(fitsIn[-128, 127, MIN, MAX], JvmByte[],
            |  fold(fitsIn[-32768, 32767, MIN, MAX], JvmShort[],
            |  fold(fitsIn[-2147483648, 2147483647, MIN, MAX], JvmInt[],
            |  fold(fitsIn[-9223372036854775808, 9223372036854775807, MIN, MAX], JvmLong[],
            |       JvmBigInteger[]))))
            |""".stripMargin
        )
      )
    case other                               => other
  }

  /** Drive the `used` traversal from a concrete `main` and return both the compilation errors and the per-`vfqn`
    * version counts of every materialised monomorphic specialization.
    */
  private def runVersioning(
      source: String,
      mainName: String = "main",
      imports: Seq[SystemImport] = systemImports
  ): IO[(Seq[TestError], Map[ValueFQN, Int])] =
    runGenerator(source, UsedNames.Key(ValueFQN(testModuleName, default(mainName))), imports)
      .map { case (errors, facts) => (toTestErrors(errors), versionCounts(facts)) }
}
