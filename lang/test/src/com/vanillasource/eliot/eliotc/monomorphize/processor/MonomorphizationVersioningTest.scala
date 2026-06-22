package com.vanillasource.eliot.eliotc.monomorphize.processor

import cats.effect.IO
import com.vanillasource.eliot.eliotc.ProcessorTest
import com.vanillasource.eliot.eliotc.ast.processor.ASTParser
import com.vanillasource.eliot.eliotc.core.processor.CoreProcessor
import com.vanillasource.eliot.eliotc.ability.processor.{
  AbilityImplementationCheckProcessor,
  AbilityImplementationProcessor,
  ModuleAbilityOverlapCheckProcessor
}
import com.vanillasource.eliot.eliotc.effect.processor.EffectDesugaringProcessor
import com.vanillasource.eliot.eliotc.matchdesugar.processor.MatchDesugaringProcessor
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.module.processor.*
import com.vanillasource.eliot.eliotc.monomorphize.fact.MonomorphicValue
import com.vanillasource.eliot.eliotc.operator.processor.OperatorResolverProcessor
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}
import com.vanillasource.eliot.eliotc.resolve.processor.ValueResolver
import com.vanillasource.eliot.eliotc.saturate.processor.SaturatedValueProcessor
import com.vanillasource.eliot.eliotc.token.Tokenizer
import com.vanillasource.eliot.eliotc.used.{UsedNames, UsedNamesProcessor}

import scala.concurrent.duration.*

/** Characterization test suite — Deliverable 0 of `docs/monomorphization-keying-plan.md`.
  *
  * These tests pin down, **empirically and before any fix**, how many monomorphic versions the compiler unrolls today
  * for each keying-plan scenario S1-S8. They are the baseline the dedup/demote/backstop fixes (Deliverables A and B)
  * are validated against: after the fix the *wins* (S1/S2/S5) must drop to their target counts, the
  * *must-not-over-merge* guards (S3/S6/S8) must stay put, and the divergent case (S7) must turn its hang into a
  * backstop error. Every assertion records the **current** behaviour (not the desired one) plus, in a comment, the
  * post-fix target — so the test fails loudly when behaviour changes and is updated as the fix lands.
  *
  * The metric is the number of distinct [[MonomorphicValue]] facts the `used` traversal materialises per `vfqn`. The
  * harness drives [[UsedNames]] from a concrete `main`, which demands a `MonomorphicValue` for every reachable
  * instantiation; counting the materialised facts grouped by `vfqn` is exactly the codegen breadth the projection
  * targets. (The plan's optional post-uncurry mirror — counting `UncurriedMonomorphicValue` — is not wired here; that
  * stage lives in the jvm backend and the breadth being deduped is already visible at the `MonomorphicValue` layer.)
  *
  * Discovery this baseline recorded (it corrected the plan's guess): the plan anticipated S1/S5 (size-indexed / reified
  * recursion) would *explode or hang*. They actually failed earlier, and '''not because of recursion''' — a *computed*
  * value (a native/def application such as `subtract(N, 1)`) passed into a type-argument / erased-parameter position was
  * not normalised before read-back, because a reified type-stack parameter's runtime body was cached as a free neutral
  * (`def bigOf[V] = V` ⇒ `bigOf[1]` stuck), so the index stayed a stuck term and surfaced as "Cannot resolve type.".
  *
  * That '''prerequisite gap is now fixed''' (`BindingProcessor.reifyingWrap`; see `ComputedTypeArgumentReadbackTest` for
  * the focused regression tests). Consequently S1 and S5 now '''unroll''' — the recursion proceeds and terminates at the
  * `fold` base case — instead of erroring: S1 ⇒ 4 versions ({3,2,1,0}), S5 ⇒ 3 versions ({3,2,1}, one fewer because its
  * arithmetic recursive branch reifies the base case to a constant and drops the `gen[0]` reference). This is the codegen
  * breadth the keying dedup/demote (Deliverables A and B) targets: S1 → 1, S5 → demote → 1. The one scenario that still
  * genuinely diverges is S7, whose growing type-argument is a structural `Box[...]` type application (not a stuck native),
  * so it quotes fine and unrolls without bound until the backstop (Deliverable A) lands.
  */
class MonomorphizationVersioningTest
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
      EffectDesugaringProcessor(),
      SaturatedValueProcessor(),
      AbilityImplementationProcessor(),
      AbilityImplementationCheckProcessor(),
      ModuleAbilityOverlapCheckProcessor(),
      SystemNativesProcessor(),
      DataTypeNativesProcessor(),
      MatchNativesProcessor(),
      UserValueNativesProcessor(),
      MonomorphicTypeCheckProcessor(),
      // A small backstop tolerance keeps the divergent scenario (S7) cheap and deterministic; every converging scenario
      // here nests at most 4 deep (S1's countdown), well under it, so the counts are unaffected.
      UsedNamesProcessor(maxNestedRepeats = 8)
    ) {

  // --- S1: recursion over a size-bounded structure (Cat 1, phantom index) ----------------------------------------
  // Intended scenario: a `sum`/`map` over a size-indexed `List[A, N]` recurses `f[N] -> f[N-1] -> ...`. The size index
  // `N` is codegen-phantom (the runtime body is a list loop, `N`-agnostic) but type-relevant, so today every step is a
  // distinct key — an O(N) explosion the plan collapses to 1. That structure is not cleanly expressible yet (no built
  // recursion + base-case dispatch), so this proxy uses a numeric index with a `fold` base case: `countdown[N]`
  // recurses to `countdown[N-1]` and folds to the base at `N <= 0`. (BigInteger constants are reified via `bigOf[V]`
  // because bare value-position literals desugar to `Int`, not `BigInteger`.)
  // PREREQUISITE GAP NOW FIXED (see `ComputedTypeArgumentReadbackTest`): the recursive step's *computed* index
  // `subtract(N, bigOf[1])` is normalised when read back as a type-argument, so the recursion now unrolls instead of
  // erroring. `countdown[3]` materialises the four distinct indices {3, 2, 1, 0} (the base case `fold` selects the
  // constant at `N <= 0`, so it terminates) ⇒ 4 versions. This is the breadth the keying dedup targets.
  // TARGET (after keying A+B): 1 (the phantom index is dropped from the codegen key; one body serves every N).
  "S1 (size-indexed recursion, phantom index)" should "unroll one version per recursive index (target after keying: 1)" in {
    runVersioning(
      "import eliot.lang.Bool\ndef bigOf[V: BigInteger]: BigInteger = V\ndef countdown[N: BigInteger]: BigInteger = fold(lessThanOrEqual(N, bigOf[0]), bigOf[0], countdown[subtract(N, bigOf[1])])\ndef main: BigInteger = countdown[3]",
      imports = intImports
    ).asserting { case (errors, counts) =>
      (errors, countOf(counts, "countdown")) shouldBe (Seq.empty, 4)
    }
  }

  // --- S2: two call sites Int[0,100] & Int[0,50] (Cat 2 representation, same width) -------------------------------
  // A single generic `id` is instantiated at two distinct bounds that share a machine representation (both fit a byte).
  // TODAY: 2 versions (the full ground type is the key, so the bounds differ). TARGET: 1 (rep-collapse — both bounds
  // canonicalise to the same width).
  "S2 (representation, same width)" should "today unroll one version per distinct bound (target after fix: 1)" in {
    runVersioning(
      "def a: Int[0, 100]\ndef b: Int[0, 50]\ndef id[A](x: A): A = x\ndef use[A, B](x: A, y: B): A = x\ndef main: Int[0, 100] = use(id(a), id(b))"
    ).asserting { case (errors, counts) =>
      (errors, countOf(counts, "id")) shouldBe (Seq.empty, 2)
    }
  }

  // --- S3: Int[0,100] & Int[0,100000] (Cat 2 representation, different width) -------------------------------------
  // Same shape as S2 but the second bound needs a wider representation than a byte, so the two instances are genuinely
  // different code. TODAY: 2. TARGET: 2 (must stay — a must-not-over-merge guard for the rep-collapse projection).
  "S3 (representation, different width)" should "unroll one version per distinct bound (target: 2, must stay)" in {
    runVersioning(
      "def a: Int[0, 100]\ndef b: Int[0, 100000]\ndef id[A](x: A): A = x\ndef use[A, B](x: A, y: B): A = x\ndef main: Int[0, 100] = use(id(a), id(b))"
    ).asserting { case (errors, counts) =>
      (errors, countOf(counts, "id")) shouldBe (Seq.empty, 2)
    }
  }

  // --- S4: reified value at K finite call sites (Cat 3, recursion-invariant) --------------------------------------
  // `tag[N]: BigInteger = N` materialises its erased `N` into a runtime constant (reification). At K=3 distinct call
  // sites each version is genuinely different code. TODAY: 3. TARGET: 3 (correct — a bounded reified family stays
  // distinct; it neither collapses nor demotes).
  "S4 (reified value, recursion-invariant)" should "unroll one version per distinct reified constant (target: K, correct)" in {
    runVersioning(
      "def tag[N: BigInteger]: BigInteger = N\ndef use[A, B, C](x: A, y: B, z: C): A = x\ndef main: BigInteger = use(tag[1], tag[2], tag[3])"
    ).asserting { case (errors, counts) =>
      (errors, countOf(counts, "tag")) shouldBe (Seq.empty, 3)
    }
  }

  // --- S5: reified value varying per recursion (Cat 3, recursion-variant) -----------------------------------------
  // Like S1 but the index is also reified into value position each step (`add(N, gen[N-1])`), so every level is
  // genuinely different code — an unbounded, non-collapsible reified family. The plan's disposition is *demote*: keep
  // the per-step value as a runtime parameter (one body, N call-site constants), not N bodies.
  // PREREQUISITE GAP NOW FIXED (see `ComputedTypeArgumentReadbackTest`): the computed recursive index normalises, so the
  // recursion unrolls instead of erroring. `gen[3]` materialises {3, 2, 1} = 3 versions: each step's value `add(N, gen[N-1])`
  // is determined entirely by the erased index, so the reification gate folds `gen[1] = add(1, gen[0]) = 1` to a constant
  // (its `gen[0]` base case fully reduces), dropping the `gen[0]` reference — one fewer version than S1's bare-reference
  // recursion. (S1's recursive branch is a plain `countdown[0]` reference, which is kept, so S1 reaches index 0.)
  // TARGET (after keying A+B): demote -> 1 body.
  "S5 (reified value, recursion-variant)" should "unroll one version per recursive index (target after keying: demote -> 1)" in {
    runVersioning(
      "import eliot.lang.Bool\ndef bigOf[V: BigInteger]: BigInteger = V\ndef gen[N: BigInteger]: BigInteger = fold(lessThanOrEqual(N, bigOf[0]), bigOf[0], add(N, gen[subtract(N, bigOf[1])]))\ndef main: BigInteger = gen[3]",
      imports = intImports
    ).asserting { case (errors, counts) =>
      (errors, countOf(counts, "gen")) shouldBe (Seq.empty, 3)
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
  // The `Box` tower `loop[A] -> loop[Box[A]] -> loop[Box[Box[A]]] -> ...` never revisits a type and has no base case,
  // so the `used` traversal would materialise versions without bound. Before Deliverable A this diverged (the IO timed
  // out). NOW: the non-convergence backstop in `UsedNamesProcessor` trips once the nested `loop` specializations exceed
  // the (here deliberately small) tolerance, converting the runaway into a specific diagnostic pointing at `loop`.
  // We assert on the backstop diagnostic specifically: this toy program also emits its own incidental errors (the
  // generic single-field accessor `unwrap` / constructor `Box` do not monomorphize cleanly in this minimal harness —
  // a pre-existing limitation, reproducible with no recursion at all, unrelated to the backstop), so the test filters
  // to the convergence error rather than over-constraining that orthogonal noise.
  "S7 (divergent recursion, no base case)" should "report a non-convergence backstop error instead of diverging" in {
    runVersioning(
      "data Box[A](unwrap: A)\ndef loop[A](x: A): A = unwrap(loop(Box(x)))\ndef bi: BigInteger\ndef main: BigInteger = loop(bi)"
    ).timeout(30.seconds).asserting { case (errors, _) =>
      errors.filter(_.message.contains("is not converging")).map(_.highlight) shouldBe Seq("loop")
    }
  }

  // --- S8: recursion on a runtime value, invariant type (control / regression guard) -----------------------------
  // A plain self-recursive value whose type never changes: `used` revisits the same key immediately and stops.
  // TODAY: 1. TARGET: 1 (must stay — ordinary recursion must not be perturbed by the projection).
  "S8 (runtime recursion, invariant type)" should "unroll exactly one version (target: 1, must stay)" in {
    runVersioning("def main: Function[BigInteger, BigInteger] = main")
      .timeout(2.seconds)
      .asserting { case (errors, counts) =>
        (errors, countOf(counts, "main")) shouldBe (Seq.empty, 1)
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
    SystemImport("Runtime", ProcessorTest.runtimeStubContent),
    SystemImport("Console", ProcessorTest.consoleStubContent),
    SystemImport("Log", ProcessorTest.logStubContent),
    SystemImport("Dep", ProcessorTest.depStubContent)
  )

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
