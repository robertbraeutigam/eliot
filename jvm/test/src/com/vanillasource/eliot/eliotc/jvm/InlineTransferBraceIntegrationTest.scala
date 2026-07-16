package com.vanillasource.eliot.eliotc.jvm

/** End-to-end proof that an **inline** return-transfer brace whose expression is ordinary Eliot arithmetic reduces
  * through the refinement channel — the payoff of the transfer-reduction linker fix
  * (`docs/refinement-channel-transfer-reduction.md`, Steps 1-4) and the resolution of its Step 5b diagnosis.
  *
  * A def `def f(a: Int, b: Int): Int {range(a) + range(b)}` carries a `^Meta` transfer companion whose body sums the
  * operand ranges through `Numeric[Interval]::+` — an *ability call*, not a native. Before the linker fix the channel
  * spliced raw callee bodies into its post-mono reduction, so this ability reference stuck and the transfer yielded ⊤;
  * the escalating executor now links only monomorphized callees, so the range propagates and a downstream `where`
  * precondition sees the real interval.
  *
  * Step 5b's premise — that the *inline* brace "silently" fails to produce the companion's compiler `SaturatedValue` —
  * does not hold for the current compiler: the dotless inline form below narrows at compile time, and the *dotted*
  * `{a.range + b.range}` form fails only at operator-precedence resolution with a **loud** error (`.` and `+` have no
  * declared relative precedence), which correctly aborts the value rather than silently accepting a wrong (or ⊤) result.
  *
  * These cases pin the *compile-time* channel behaviour (fact production + narrowing), which is what Step 5b concerns.
  * Runtime lowering of a narrow return across a *user* def's call boundary is a separate backend representation matter
  * (the callee's body is compiled once with ⊤ parameters, so it returns a wide representation the brace-narrowed caller
  * does not match) — out of Step 5's front-end scope and not reachable from the shipped layers, whose transfer braces
  * sit only on native leaves.
  */
class InlineTransferBraceIntegrationTest extends FullIntegrationTest {
  private val prelude =
    """import eliot.effect.Console
      |import eliot.lang.Interval
      |import eliot.lang.Numeric
      |def useByte(x: Int): Int where withinByte(range(x)) = x
      |def f(a: Int, b: Int): Int {range(a) + range(b)} = a + b
      |""".stripMargin

  "an inline `+` transfer brace" should "narrow an out-of-range sum so the where precondition is rejected" in {
    compileForErrors(prelude + "def main: IO[Unit] = printLine(intToString(useByte(f(100, 100))))")
      .asserting(_ should include("precondition of 'Test::useByte' is not satisfied"))
  }

  it should "stay ⊤ for an unknown operand rather than bogus-narrow (soundness)" in {
    compileForErrors(
      prelude + "def relay(y: Int): Int = useByte(f(y, y))\ndef main: IO[Unit] = printLine(intToString(relay(10)))"
    ).asserting(_ should include("Cannot prove the precondition of 'Test::useByte'"))
  }

  "a dotted `a.range + b.range` transfer brace" should "fail loudly on undeclared `.`/`+` precedence, not silently" in {
    compileForErrors(
      prelude.replace("{range(a) + range(b)}", "{a.range + b.range}") +
        "def main: IO[Unit] = printLine(intToString(useByte(f(100, 100))))"
    ).asserting(_ should include("have no defined relative precedence"))
  }
}
