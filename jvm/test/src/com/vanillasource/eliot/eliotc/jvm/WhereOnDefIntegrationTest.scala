package com.vanillasource.eliot.eliotc.jvm

/** End-to-end proof of `where`-on-defs — a refinement precondition on an ordinary `def`, verified at each use site by
  * the refinement channel (`docs/bounds-as-refinements.md` §4.3). `def useByte(x: Int): Int where withinByte(range(x))`
  * demands that every call's argument have a value range provably within a signed byte; the channel reduces the
  * generated `^Where` companion over the argument's computed interval and rejects a caller whose range is out of bounds
  * or unknown (⊤). This is the use-site verification that closes the Step-6 enforcement gap (out-of-range `Int` values
  * had no JVM-backed rejection until now).
  */
class WhereOnDefIntegrationTest extends FullIntegrationTest {
  private val useByte =
    """import eliot.effect.Console
      |import eliot.lang.Interval
      |def useByte(x: Int): Int where withinByte(range(x)) = x
      |""".stripMargin

  "a where precondition" should "accept a call whose argument range provably fits" in {
    compileAndRun(useByte + "def main: IO[Unit] = printLine(intToString(useByte(42)))")
      .asserting(_ shouldBe "42")
  }

  it should "accept an in-range literal even inside a parametered def's body" in {
    compileAndRun(useByte + "def wrap(ignored: Int): Int = useByte(127)\ndef main: IO[Unit] = printLine(intToString(wrap(0)))")
      .asserting(_ shouldBe "127")
  }

  it should "reject a call whose argument range exceeds the bound" in {
    compileForErrors(useByte + "def main: IO[Unit] = printLine(intToString(useByte(1000)))")
      .asserting(_ should include("precondition of 'Test::useByte' is not satisfied"))
  }

  it should "reject a call whose argument range is unknown (top), rather than silently accept" in {
    compileForErrors(
      useByte + "def relay(y: Int): Int = useByte(y)\ndef main: IO[Unit] = printLine(intToString(relay(42)))"
    ).asserting(_ should include("Cannot prove the precondition of 'Test::useByte'"))
  }
}
