package com.vanillasource.eliot.eliotc.jvm

/** **Constraint-aware declination** (`docs/testing-effects.md` L1): a structurally-matching implementation whose `~`
  * type-parameter constraints have no implementation at the matched bindings **declines**, instead of surviving
  * selection and colliding with the implementation that does apply.
  *
  * This is what extends carrier substitution — the testing strategy — from application-owned abilities to the
  * standard-library effects anyone actually wants to fake. The jvm layer implements every one of them with a
  * constrained catch-all (`implement[F[_] ~ Suspend] Console[F]`, and the same for `Log`/`FileSystem`/`Process`/
  * `Environment`), which matches *every* carrier structurally; before this, a test's own instance for its own pure
  * carrier was a second surviving candidate and the query was ambiguous.
  *
  * Run end to end over the real base layer, because the point is precisely how the *stdlib's* instances behave.
  */
class AbilityConstraintDeclinationTest extends FullIntegrationTest {

  /** The pure test carrier and its fake `Console`, threading the transcript written so far. It has no `Suspend`
    * instance, which is both why the jvm candidate declines for it and why it could not perform real I/O if it tried.
    */
  private val recordedCarrier =
    """import eliot.carrier.Effect
      |
      |data Recorded[A](runRecorded: Function[String, Pair[A, String]])
      |
      |implement Effect[Recorded] {
      |   def pure[A](a: A): Recorded[A] = Recorded(w -> Pair(a, w))
      |
      |   def flatMap[A, B](f: Function[A, Recorded[B]], fa: Recorded[A]): Recorded[B] =
      |      Recorded(w -> foldPair(a -> w2 -> runRecorded(f(a))(w2), runRecorded(fa)(w)))
      |
      |   def map[A, B](f: Function[A, B], fa: Recorded[A]): Recorded[B] =
      |      Recorded(w -> foldPair(a -> w2 -> Pair(f(a), w2), runRecorded(fa)(w)))
      |}
      |""".stripMargin

  private val fakeConsole =
    """implement Console[Recorded] {
      |   def printLine(s: String): Recorded[Unit] = Recorded(w -> Pair(unit, w ++ s ++ ";"))
      |
      |   def readLine: Recorded[Option[String]] = Recorded(w -> Pair(None, w))
      |}
      |""".stripMargin

  /** Production code, identical in every program here: it declares `{Console}`, names no carrier, and is never edited
    * for the test — the whole claim of the strategy.
    */
  private val production =
    """def greeting(name: String): {Console} Unit = printLine("Hello, " ++ name ++ "!")
      |""".stripMargin

  "a fake instance of a standard-library effect" should "interpret untouched production code" in {
    compileAndRun(
      s"""$recordedCarrier
         |$fakeConsole
         |$production
         |def transcript: Pair[Unit, String] = runRecorded(greeting("Bob"))("")
         |
         |def main: {Console} Unit = printLine(second(transcript))
         |""".stripMargin
    ).asserting(_ shouldBe "Hello, Bob!;")
  }

  // The instance is colocated with the *carrier*, not with `Console` — the orphan rule's second placement. Effect
  // accounting used to read the performed ability's module off the reference itself, which is the ability's module only
  // for the first placement, so `greeting` at `Recorded` derived an effect that did not match the `{Console}` it
  // declares. The ability an implementation implements is now read off its own declaration.
  it should "still satisfy the value's declared effect row when colocated with the carrier" in {
    compileForErrors(
      s"""$recordedCarrier
         |$fakeConsole
         |$production
         |def transcript: Pair[Unit, String] = runRecorded(greeting("Bob"))("")
         |
         |def main: {Console} Unit = printLine(second(transcript))
         |""".stripMargin
    ).asserting(_ should not include "performs the effect 'Console'")
  }

  // The other half of the claim: nothing changes in production, because a real carrier does have `Suspend`. The same
  // fake instance is present and declines here — it matches only `Recorded`.
  it should "leave the real instance resolving for a real carrier in the same program" in {
    compileAndRun(
      s"""$recordedCarrier
         |$fakeConsole
         |$production
         |def main: {Console} Unit = greeting("World")
         |""".stripMargin
    ).asserting(_ shouldBe "Hello, World!")
  }

  // W3, the **capture tag**: `{| Recorded} A` is the pinned row at zero entries — the same type as `Recorded[A]`,
  // tagged as a slot that *hosts a computation on that carrier*. Without it the run had to sit in a definition with no
  // ambient carrier of its own, because a region writes every carrier-generic callee at its own carrier; `main`
  // declares `{Console}` and so is a region. With the tag the slot is a capture, the elaborator writes nothing into it,
  // and the checker instantiates the body at the declared `Recorded`.
  it should "run a fake carrier inline, at a call site inside a region of its own" in {
    compileAndRun(
      s"""$recordedCarrier
         |$fakeConsole
         |$production
         |def onRecorded(body: {| Recorded} Unit): String = second(runRecorded(body)(""))
         |
         |def main: {Console} Unit = printLine(onRecorded(greeting("Bob")))
         |""".stripMargin
    ).asserting(_ shouldBe "Hello, Bob!;")
  }

  // The same, with a *block* — several statements on the fake carrier, still written inline. This is the shape a test
  // framework wants, and it needed both halves: the tag to fix the carrier, and the block peel (§3.3) so the harness is
  // not charged for the effect it fakes.
  it should "run a multi-statement block on a fake carrier inline" in {
    compileAndRun(
      s"""$recordedCarrier
         |$fakeConsole
         |$production
         |def onRecorded(body: {| Recorded} Unit): String = second(runRecorded(body)(""))
         |
         |def main: {Console} Unit = printLine(onRecorded({
         |   greeting("Ann")
         |   greeting("Bob")
         |}))
         |""".stripMargin
    ).asserting(_ shouldBe "Hello, Ann!;Hello, Bob!;")
  }

  // The tag fixes the carrier the *declaration* names and nothing else: a body performing an effect that carrier has no
  // instance for is rejected at the use site, never silently rerouted to the real one.
  it should "reject a captured body performing an effect the named carrier cannot supply" in {
    compileForErrors(
      s"""$recordedCarrier
         |$production
         |def onRecorded(body: {| Recorded} Unit): String = second(runRecorded(body)(""))
         |
         |def main: {Console} Unit = printLine(onRecorded(greeting("Bob")))
         |""".stripMargin
    ).asserting(_ should include("Console"))
  }

  // Declining is not resolving: a carrier with no `Console` instance of its own has no candidate left once the jvm
  // catch-all declines, and the demand fails at the use site rather than silently picking the real instance.
  it should "resolve nothing for a pure carrier that implements no Console at all" in {
    compileForErrors(
      s"""$recordedCarrier
         |$production
         |def transcript: Pair[Unit, String] = runRecorded(greeting("Bob"))("")
         |
         |def main: {Console} Unit = printLine(second(transcript))
         |""".stripMargin
    ).asserting(_ should include("Console"))
  }
}
