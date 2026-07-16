package com.vanillasource.eliot.eliotc.jvm

/** End-to-end proof of `where`-on-defs — a refinement precondition on an ordinary `def`, verified at each use site by
  * the refinement channel (`docs/bounds-as-refinements.md` §4.3). `def useByte(x: Int): Int where withinByte(range(x))`
  * demands that every call's argument have a value range provably within a signed byte; the channel reduces the
  * generated `^Where` companion over the argument's computed interval and rejects a caller whose range is out of bounds
  * or unknown (⊤). This is the use-site verification that closes the Step-6 enforcement gap (out-of-range `Int` values
  * had no JVM-backed rejection until now).
  */
class WhereOnDefIntegrationTest extends FullIntegrationTest {
  // `withinByte` is a test-local predicate (it deliberately lives only where a test needs it, not in any layer).
  private val withinByte =
    """|def byteMin: BigInteger = -128
       |def byteMax: BigInteger = 127
       |def withinByte(i: Interval[BigInteger]): Bool = lessThanOrEqual(byteMin, start(i)) && lessThanOrEqual(end(i), byteMax)
       |""".stripMargin

  private val useByte =
    "import eliot.effect.Console\n" + withinByte +
      "def useByte(x: Int): Int where withinByte(range(x)) = x\n"

  "a where precondition" should "accept a call whose argument range provably fits" in {
    compileAndRun(useByte + "def main: IO[Unit] = printLine(show(useByte(42)))")
      .asserting(_ shouldBe "42")
  }

  it should "accept an in-range literal even inside a parametered def's body" in {
    compileAndRun(useByte + "def wrap(ignored: Int): Int = useByte(127)\ndef main: IO[Unit] = printLine(show(wrap(0)))")
      .asserting(_ shouldBe "127")
  }

  it should "reject a call whose argument range exceeds the bound" in {
    compileForErrors(useByte + "def main: IO[Unit] = printLine(show(useByte(1000)))")
      .asserting(_ should include("precondition of 'Test::useByte' is not satisfied"))
  }

  it should "reject a call whose argument range is unknown (top), rather than silently accept" in {
    compileForErrors(
      useByte + "def relay(y: Int): Int = useByte(y)\ndef main: IO[Unit] = printLine(show(relay(42)))"
    ).asserting(_ should include("Cannot prove the precondition of 'Test::useByte'"))
  }

  // Higher-order escape (`docs/refinement-channel-follow-ups.md` §2.1): passing a `where`-bearing def as a *value*
  // rides a function value whose eventual call the channel never sees, so the precondition would be silently skipped.
  // Reject any reference to a `where`-bearing def that is not the head of a full application.
  it should "reject a where-bearing def passed as a bare value, not silently bypass the precondition" in {
    compileForErrors(
      useByte +
        "def call(f: Int => Int, x: Int): Int = f(x)\n" +
        "def main: IO[Unit] = printLine(show(call(useByte, 1000)))"
    ).asserting(_ should include("cannot be passed as a value"))
  }

  it should "reject a partial application of a where-bearing def passed as a value" in {
    compileForErrors(
      "import eliot.effect.Console\n" + withinByte +
        "def clampFirst(a: Int, b: Int): Int where withinByte(range(a)) = a\n" +
        "def apply1(g: Int => Int): Int = g(5)\n" +
        "def main: IO[Unit] = printLine(show(apply1(clampFirst(200))))"
    ).asserting(_ should include("cannot be passed as a value"))
  }
}
