package com.vanillasource.eliot.eliotc.jvm

/** A definition gives a computation it was handed only what it has (`docs/effects.md` D20 rule 5). A row-typed slot's
  * caller writes the actual's bindings from the declaration, and an entry the slot supplies by `Default` is a promise
  * that a frame for it is entered before the actual runs. Only a callee's slot that supplies the entry — a discharger
  * the computation is passed on to — keeps that promise; a platform primitive, being body-less, is where every such
  * frame starts. The witness for §8 item 13 is the first case.
  */
class GivenEffectIntegrationTest extends FullIntegrationTest {

  private val recording =
    """import eliot.effect.Console
      |import eliot.effect.Writer
      |
      |implement recordingConsole: Console {
      |   def printLine(s: String): {Writer[String]} Unit = tell(s ++ ";")
      |   def readLine: Option[String] = None
      |}
      |""".stripMargin

  // §8 item 13: the slot supplied the platform console by `Default`, so a pure signature performed real I/O.
  "a definition running a computation whose effect it has nothing to give" should "be rejected" in {
    compileForErrors(
      """import eliot.effect.Console
        |
        |def launder(body: {Console} Unit): Unit = body
        |
        |def main: {Console} Unit = launder(printLine("hi"))
        |""".stripMargin
    ).asserting(
      _ should include(
        ":3:43:'body' is given the effect 'Console' here, which this definition has no implementation of to give."
      )
    )
  }

  "a definition passing the computation on to a discharger" should "give that discharger's frame" in {
    compileAndRun(
      """import eliot.effect.Console
        |import eliot.effect.Abort
        |
        |def attempt(body: {Abort} String): String = body else "none"
        |
        |def main: {Console} Unit = printLine(attempt(if(false, "some")))
        |""".stripMargin
    ).asserting(_ shouldBe "none")
  }

  "a `with` in the body" should "not give the computation anything" in {
    compileForErrors(
      recording +
        """
          |def launder(body: {Console} Unit): String = runWriterToLog(body with recordingConsole)
          |
          |def main: {Console} Unit = printLine(launder(printLine("hi")))
          |""".stripMargin
    ).asserting(_ should include("'body' is given the effect 'Console' here"))
  }

  "a slot's named implementation" should "be given the effects its clauses perform" in {
    compileAndRun(
      recording +
        """
          |def transcriptOf(program: {Console} Unit with recordingConsole): String = runWriterToLog(program)
          |
          |def main: {Console} Unit = printLine(transcriptOf(printLine("hi")))
          |""".stripMargin
    ).asserting(_ shouldBe "hi;")
  }

  it should "be rejected when the definition does not give them" in {
    compileForErrors(
      recording +
        """
          |def silently(program: {Console} Unit with recordingConsole): Unit = program
          |
          |def main: {Console} Unit = silently(printLine("hi"))
          |""".stripMargin
    ).asserting(_ should include("'program' is given the effect 'Writer' here"))
  }

  "a computation promised the default" should "not be passed to a slot binding another implementation" in {
    compileForErrors(
      recording +
        """
          |def transcriptOf(program: {Console} Unit with recordingConsole): String = runWriterToLog(program)
          |def relay(program: {Console} Unit): String = transcriptOf(program)
          |
          |def main: {Console} Unit = printLine(relay(printLine("hi")))
          |""".stripMargin
    ).asserting(
      _ should include(
        "'program' was promised the default 'Console', but runs where a slot binds another implementation for it."
      )
    )
  }
}
