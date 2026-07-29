package com.vanillasource.eliot.eliotc.jvm

/** The permanent behavioural gate over [[EffectCorpus]]: the worked effect shapes must keep compiling **and printing
  * the same thing** over the real `lang`/`stdlib`/`jvm` layers.
  *
  * The corpus was introduced as the input of two effects-as-rows experiments — the R3 shadow sweep (row-check every
  * demanded definition and compare the verdict against v2) and the R4 shadow compile (re-compile from pre-elaborated
  * facts, with run A's output as the oracle). Both are gone: v2 no longer exists to agree with, the row verification
  * they shadowed is wired into the pipeline (A.11.6), and the elaboration they injected is the pipeline (A.11.5). What
  * their oracle actually pinned — that this program runs and prints exactly this — was never written down as an
  * assertion, only compared against itself; here it is, spelled out (docs/effects-as-rows.md A.11.9).
  */
class EffectCorpusIntegrationTest extends FullIntegrationTest {

  // In order: the `{Console}` block (`a`/`b`), the identity-handler `catch` under a pure return (`unparseable`), the
  // `runStateToPair` discharge under a pure return (`initial`), the effectful handler (`boom`), the two `runAbort`
  // discharges through the program's own `Id` carrier (`granted`/`DENIED`), and the State program at `IO` (`before`).
  "the combined effect corpus" should "run every worked shape and print their results in order" in {
    compileAndRun(EffectCorpus.combinedProgram).asserting(
      _ shouldBe
        """a
          |b
          |unparseable
          |initial
          |boom
          |granted
          |DENIED
          |before""".stripMargin
    )
  }

  // The `>` prefix on all three elements from a single supplied line is the assertion: `readLine` is written once,
  // outside the traversal, so it runs once — a deferred construction would read per element instead.
  "an effect performed while building an argument" should "run where it is written, once" in {
    compileAndRun(EffectCorpus.argumentConstructionProgram, stdin = ">\n").asserting(
      _ shouldBe
        """host
          |none
          |>a
          |>b
          |>c
          |pure:a
          |pure:b
          |pure:c""".stripMargin
    )
  }

  "repeated readLine" should "read successive lines, not end-of-stream after the first" in {
    compileAndRun(EffectCorpus.stdinProgram, stdin = "alpha\nbeta\ngamma\n").asserting(
      _ shouldBe
        """start
          |1:alpha
          |2:beta
          |3:gamma""".stripMargin
    )
  }

  "the Inf corpus program" should "loop forever, printing its line repeatedly" in {
    compileAndRunBounded(EffectCorpus.infProgram, timeoutMillis = 400)
      .asserting(_.linesIterator.count(_ == "tick") should be > 5)
  }
}
