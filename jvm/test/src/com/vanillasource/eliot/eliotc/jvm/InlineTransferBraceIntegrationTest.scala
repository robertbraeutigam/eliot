package com.vanillasource.eliot.eliotc.jvm

/** End-to-end proof that an **inline** return-transfer brace whose expression is ordinary Eliot arithmetic reduces
  * through the refinement channel — the payoff of the transfer-reduction linker fix and the resolution of its "5b"
  * inline-brace diagnosis (both recorded in `docs/refinement-channel-follow-ups.md` §1).
  *
  * A def `def f(a: Int, b: Int): Int {range(a) + range(b)}` carries a `^Meta` transfer companion whose body sums the
  * operand ranges through `Numeric[Interval]::+` — an *ability call*, not a native. Before the linker fix the channel
  * spliced raw callee bodies into its post-mono reduction, so this ability reference stuck and the transfer yielded ⊤;
  * the escalating executor now links only monomorphized callees, so the range propagates and a downstream `where`
  * precondition sees the real interval.
  *
  * That diagnosis's premise — that the *inline* brace "silently" fails to produce the companion's compiler
  * `SaturatedValue` — does not hold for the current compiler: both the dotless inline form and the *dotted*
  * `{a.range + b.range}` form below narrow at compile time. The dotted form resolves because `.` is declared
  * `below apply` (so it shares `apply`'s precedence island and binds tighter than the floating `+`); being
  * subject-last, `a.range + b.range` then reads as `range(a) + range(b)` — the identical sum the dotless brace
  * computes, and it narrows the same way rather than silently accepting a wrong (or ⊤) result.
  *
  * These cases pin the *compile-time* channel behaviour (fact production + narrowing). Runtime lowering of a narrow
  * return across a *user* def's call boundary is a separate backend representation matter
  * (the callee's body is compiled once with ⊤ parameters, so it returns a wide representation the brace-narrowed caller
  * does not match) — deferred in `docs/refinement-channel-follow-ups.md` §4 and not reachable from the shipped layers,
  * whose transfer braces sit only on native leaves.
  */
class InlineTransferBraceIntegrationTest extends FullIntegrationTest {
  // `withinByte` is a test-local predicate (it deliberately lives only where a test needs it, not in any layer).
  private val prelude =
    """import eliot.jvm.IO
      |import eliot.effect.Console
      |def withinByte(i: Interval[BigInteger]): Bool = rangeWithin[0, 127](i)
      |def useByte(x: Int): Int where withinByte(range(x)) = x
      |def f(a: Int, b: Int): Int {range(a) + range(b)} = a + b
      |""".stripMargin

  "an inline `+` transfer brace" should "narrow an out-of-range sum so the where precondition is rejected" in {
    compileForErrors(prelude + "def main: IO[Unit] = printLine(show(useByte(f(100, 100))))")
      .asserting(_ should include("precondition of 'Test::useByte' is not satisfied"))
  }

  it should "stay ⊤ for an unknown operand rather than bogus-narrow (soundness)" in {
    compileForErrors(
      prelude + "def relay(y: Int): Int = useByte(f(y, y))\ndef main: IO[Unit] = printLine(show(relay(10)))"
    ).asserting(_ should include("Cannot prove the precondition of 'Test::useByte'"))
  }

  // A brace is ordinary compiler-track Eliot, so it may call an ordinary generic helper — including one whose body
  // applies a data constructor to its own parameter (`closed[T](s, e) = Interval(Bounded(s), Bounded(e))`). Such a
  // helper reifies no binder, so `BindingClosure` wraps its body in no type-argument lambda; the checker's own body
  // reduction nevertheless writes the solved type argument, and applying it used to land `BigInteger` in `s`'s slot and
  // shift every value argument one left — reducing a well-typed brace to an ill-typed range with nothing downstream in a
  // position to notice (`SemValue.VTopDef.typeBinders`). These two pin both directions of the reduced range.
  private val viaHelper =
    """import eliot.jvm.IO
      |import eliot.effect.Console
      |def withinByte(i: Interval[BigInteger]): Bool = rangeWithin[0, 127](i)
      |def useByte(x: Int): Int where withinByte(range(x)) = x
      |def byteRange[Lo: BigInteger, Hi: BigInteger]: Interval[BigInteger] = closed(Lo, Hi)
      |def openRange[Lo: BigInteger]: Interval[BigInteger] = atLeast(Lo)
      |""".stripMargin

  // Compile-time only, like the rest of this file: runtime lowering of a brace-narrowed return across a *user* def's
  // call boundary is the separate, deferred representation matter noted in the class comment.
  "a transfer brace routed through a user-defined generic constructor helper" should "state the range the helper builds" in {
    compileForErrors(
      viaHelper + "def clamped(a: Int): Int {byteRange[0, 127]} = a\ndef main: IO[Unit] = printLine(show(useByte(clamped(100))))"
    ).asserting(_ should not include "precondition")
  }

  it should "narrow to the helper's actual endpoints, not merely to something" in {
    compileForErrors(
      viaHelper + "def wide(a: Int): Int {byteRange[0, 200]} = a\ndef main: IO[Unit] = printLine(show(useByte(wide(100))))"
    ).asserting(_ should include("precondition of 'Test::useByte' is not satisfied"))
  }

  it should "keep a half-open endpoint half-open, so the byte precondition is rejected" in {
    compileForErrors(
      viaHelper + "def open(a: Int): Int {openRange[0]} = a\ndef main: IO[Unit] = printLine(show(useByte(open(100))))"
    ).asserting(_ should include("precondition of 'Test::useByte' is not satisfied"))
  }

  // `.` is `below apply`, so it shares `apply`'s precedence island and binds tighter than the floating `+`; being
  // subject-last, `a.range` ≡ `range(a)`, so the dotted brace reads exactly as `{range(a) + range(b)}` and narrows
  // identically — where before it aborted at precedence resolution with "no defined relative precedence".
  "a dotted `a.range + b.range` transfer brace" should "narrow via subject-last `.` binding tighter than `+`" in {
    compileForErrors(
      prelude.replace("{range(a) + range(b)}", "{a.range + b.range}") +
        "def main: IO[Unit] = printLine(show(useByte(f(100, 100))))"
    ).asserting(_ should include("precondition of 'Test::useByte' is not satisfied"))
  }
}
