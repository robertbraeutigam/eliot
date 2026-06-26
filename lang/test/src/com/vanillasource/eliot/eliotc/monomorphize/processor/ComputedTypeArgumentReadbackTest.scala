package com.vanillasource.eliot.eliotc.monomorphize.processor

import cats.effect.IO
import com.vanillasource.eliot.eliotc.ProcessorTest
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.monomorphize.fact.{GroundValue, MonomorphicValue}
import com.vanillasource.eliot.eliotc.plugin.LangProcessors
import com.vanillasource.eliot.eliotc.stdlib.plugin.StdlibNativesProcessor
import com.vanillasource.eliot.eliotc.used.UsedNames

/** Regression suite for the '''computed type-argument read-back''' fix (prerequisite of the
  * monomorphization-keying plan).
  *
  * A type-stack ('''generic''') parameter that a value reifies — references in value position — is a binder of the
  * value's '''signature''' but '''not''' of its runtime body: `def bigOf[V] = V` has signature `(V) -> BigInteger` yet
  * runtime body just `V`. The monomorphize binding cache used to evaluate that body under the empty environment, so the
  * reified `V` became a free neutral and `bigOf[1]` only appended `1` to the neutral's spine instead of reducing to `1`.
  * Any '''computed''' type argument flowing through such a value — `h[subtract(N, bigOf[1])]`, a native applied to a
  * reified constant — therefore stayed a stuck term and failed read-back with "Cannot resolve type.", which blocked
  * every size-indexed / reified recursion the keying plan targets (its scenarios S1 and S5).
  *
  * `BindingProcessor.reifyingWrap` now wraps the cached body in lambda binders for the leading type-stack parameters the
  * body reifies, so applying the explicit type arguments substitutes them (exactly as the checker does for the value's
  * own monomorphization). These tests pin that down directly: the computed indices now normalise, no "Cannot resolve
  * type." error is produced, and the downstream specializations carry the '''reduced''' type arguments.
  */
// Composes `StdlibNativesProcessor` (the stdlib-layer `subtract`/`lessThanOrEqual`/… natives the computed indices
// reduce through) onto `LangProcessors` and registers its native label so the binding merger consults it.
class ComputedTypeArgumentReadbackTest
    extends ProcessorTest(
      (LangProcessors(extraNativeBindingLabels = Seq(StdlibNativesProcessor.stdlibLabel)) :+ StdlibNativesProcessor())*
    ) {

  // A reified BigInteger value `bigOf[1]` applied to a literal, used directly as another value's type argument, must
  // reduce to the constant `1` (it was previously a stuck neutral). `box[N] = N` is specialized at the reduced index.
  "a reified value applied to a literal" should "reduce in a type-argument position" in {
    runReadback(
      "def bigOf[V: BigInteger]: BigInteger = V\ndef box[N: BigInteger]: BigInteger = N\ndef main: BigInteger = box[bigOf[5]]"
    ).asserting { case (errors, args) =>
      (errors, indicesOf(args, "box")) shouldBe (Seq.empty, Set(BigInt(5)))
    }
  }

  // A *computed* (native-applied) type argument that flows through a reified value — `h[subtract(N, bigOf[1])]` — must
  // normalise before read-back. Non-recursive isolation: `g[3]` calls `h[subtract(3, 1)]`, so `h` is specialized at the
  // reduced index `2`, with no "Cannot resolve type." error.
  "a computed type argument through a reified value" should "normalise before read-back" in {
    runReadback(
      "def bigOf[V: BigInteger]: BigInteger = V\ndef h[N: BigInteger]: BigInteger = bigOf[N]\ndef g[N: BigInteger]: BigInteger = h[subtract(N, bigOf[1])]\ndef main: BigInteger = g[3]"
    ).asserting { case (errors, args) =>
      (errors, indicesOf(args, "h")) shouldBe (Seq.empty, Set(BigInt(2)))
    }
  }

  // This `countdown` proxy once exercised the read-back fix across a recursive unroll {3, 2, 1, 0}. The read-back fix
  // itself is covered non-recursively above (`g[3]` -> `h[subtract(3, 1)]`); `countdown` refers back to itself, so
  // termination M1 now rejects the value cycle outright.
  "a size-indexed recursion with a computed index" should "be rejected as recursion" in {
    runReadback(
      "import eliot.lang.Bool\ndef bigOf[V: BigInteger]: BigInteger = V\ndef countdown[N: BigInteger]: BigInteger = fold(lessThanOrEqual(N, bigOf[0]), bigOf[0], countdown[subtract(N, bigOf[1])])\ndef main: BigInteger = countdown[3]"
    ).asserting { case (errors, _) =>
      errors.map(_.message) should contain("Value 'countdown' is defined recursively.")
    }
  }

  // Regression guard for the `reifyingWrap` change: an ordinary generic whose type parameter is *not* reified (`A` never
  // appears in value position) must keep working with an implicit type argument — its cached body is left unwrapped, so
  // `id(x)` still applies `x` to the value lambda rather than consuming it as `A`.
  "an ordinary generic with a non-reified type parameter" should "still apply with an implicit type argument" in {
    runReadback(
      "def id[A](x: A): A = x\ndef one[V: BigInteger]: BigInteger = V\ndef main: BigInteger = id(one[7])"
    ).asserting { case (errors, args) =>
      (errors, args.contains(ValueFQN(testModuleName, default("id")))) shouldBe (Seq.empty, true)
    }
  }

  // --- Helpers ---------------------------------------------------------------------------------------------------

  /** Drive the `used` traversal from a concrete `main` and return the compilation errors plus, per `vfqn`, the set of
    * materialized [[MonomorphicValue]] specializations (their full ground type arguments).
    */
  private def runReadback(source: String): IO[(Seq[TestError], Map[ValueFQN, Set[Seq[GroundValue]]])] =
    runGenerator(source, UsedNames.Key(ValueFQN(testModuleName, default("main"))), readbackImports)
      .map { case (errors, facts) =>
        val versions = facts.values
          .collect { case mv: MonomorphicValue => mv }
          .groupBy(_.vfqn)
          .view
          .mapValues(_.map(_.typeArguments).toSet)
          .toMap
        (toTestErrors(errors), versions)
      }

  /** The first-type-argument [[BigInt]] index of every specialization of `name` — the reduced recursion / reification
    * index each scenario asserts on.
    */
  private def indicesOf(versions: Map[ValueFQN, Set[Seq[GroundValue]]], name: String): Set[BigInt] =
    versions.getOrElse(ValueFQN(testModuleName, default(name)), Set.empty).collect {
      case Seq(GroundValue.Direct(v: BigInt, _), _*) => v
    }

  /** Ambient stubs plus the `BigInteger` arithmetic (`subtract`/`add`/`lessThanOrEqual`) and the `Bool` `fold` the
    * reified / size-indexed scenarios reference. `Int`/`Runtime`/`Console`/`Log`/`Dep` are in `defaultSystemModules`, so
    * their stubs must be present even though these tests compute on `BigInteger` directly.
    */
  private val readbackImports: Seq[SystemImport] = Seq(
    SystemImport("Function", "type Function[A, B]\ndef apply[A, B](f: Function[A, B], a: A): B"),
    SystemImport("Type", "type Type"),
    SystemImport(
      "BigInteger",
      "import eliot.lang.Bool\ntype BigInteger\ndef lessThanOrEqual(a: BigInteger, b: BigInteger): Bool\ndef add(a: BigInteger, b: BigInteger): BigInteger\ndef subtract(a: BigInteger, b: BigInteger): BigInteger"
    ),
    SystemImport("Unit", "type Unit"),
    SystemImport("String", "type String"),
    SystemImport("IO", "type IO"),
    SystemImport("PatternMatch", ""),
    SystemImport("TypeMatch", ""),
    SystemImport(
      "Bool",
      "type Bool\ndef true: Bool\ndef false: Bool\ndef fold[A](condition: Bool, whenTrue: A, whenFalse: A): A"
    ),
    SystemImport("Int", ProcessorTest.intStubContent),
    SystemImport("Runtime", ProcessorTest.runtimeStubContent),
    SystemImport("Console", ProcessorTest.consoleStubContent),
    SystemImport("Log", ProcessorTest.logStubContent),
    SystemImport("Dep", ProcessorTest.depStubContent)
  )
}
