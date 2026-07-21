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
      |def byteMin: BigInteger = -128
      |def byteMax: BigInteger = 127
      |def withinByte(i: Interval[BigInteger]): Bool = lessThanOrEqual(byteMin, start(i)) && lessThanOrEqual(end(i), byteMax)
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
