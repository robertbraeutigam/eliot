package com.vanillasource.eliot.eliotc.jvm

/** The permanent effect-system corpus: Eliot programs that exercise the design's worked effect shapes end to end over
  * the real `lang`/`stdlib`/`jvm` layers.
  *
  * This is a **fixture, not a harness** — it deliberately depends on nothing, which is what let the experiment
  * harnesses that read it (the R3 shadow sweep, the R4 shadow compile) be deleted at A.11.9 without taking the corpus
  * with them. Its permanent consumer is [[EffectCorpusIntegrationTest]], which pins what these programs print. Every
  * effect shape the project has had to keep working lives here; add to it rather than inlining a new program into a
  * suite.
  */
object EffectCorpus {

  /** One combined program covering the design's worked shapes: direct-style Console blocks, discharge-to-pure (`catch` +
    * `runStateToPair` under pure returns), an effectful catch handler, a carrier-polymorphic Abort program under a local
    * pure Id carrier (with a hand-written `Effect` instance), the State effect at a concrete IO carrier, and a
    * run-carrier-headed `main` sequencing them all (so demand-driven compilation reaches every definition).
    */
  val combinedProgram: String =
    """import eliot.jvm.IO
      |import eliot.effect.Console
      |import eliot.carrier.Effect
      |import eliot.effect.Throw
      |import eliot.effect.State
      |import eliot.effect.Abort
      |
      |data Id[A](runId: A)
      |
      |implement Effect[Id] {
      |   def pure[A](a: A): Id[A] = Id(a)
      |   def flatMap[A, B](f: Function[A, Id[B]], fa: Id[A]): Id[B] = f(runId(fa))
      |   def map[A, B](f: Function[A, B], fa: Id[A]): Id[B] = Id(f(runId(fa)))
      |}
      |
      |def shout(s: String): {Console} Unit = printLine(s)
      |
      |def greet: {Console} Unit = {
      |   shout("a")
      |   printLine("b")
      |}
      |
      |def parsed(raw: String): {Throw[String]} String = raise("unparseable")
      |
      |def recovered: String = parsed("x") catch (err -> err)
      |
      |def counted: Pair[String, String] = runStateToPair("initial", state)
      |
      |def failUnit: {Throw[String]} Unit = raise("boom")
      |
      |def caught: {Console} Unit = failUnit catch (err -> printLine(err))
      |
      |def allowed: {Abort} String = "granted"
      |def denied: {Abort} String = abort
      |
      |def testAllowed: Option[String] = runId(runAbort(allowed))
      |def testDenied: Option[String] = runId(runAbort(denied))
      |
      |def swap(next: String): {State[String]} String =
      |   flatMap(old -> flatMap(ignored -> pure(old), putState(next)), state)
      |
      |def prog: IO[Pair[String, String]] = runStateToPair("before", swap("after"))
      |
      |def main: IO[Unit] = {
      |   greet
      |   printLine(recovered)
      |   printLine(counted.first)
      |   caught
      |   printLine(foldOption("DENIED", s -> s, testAllowed))
      |   printLine(foldOption("DENIED", s -> s, testDenied))
      |   flatMap(pair -> printLine(pair.first), prog)
      |}""".stripMargin

  /** A deliberately non-terminating program: `Inf` is an ordinary row entry riding the same union. Compile-only. */
  val infProgram: String =
    """import eliot.jvm.IO
      |import eliot.effect.Console
      |import eliot.effect.Inf
      |
      |def main: {Inf, Console} Unit = forever(printLine("tick"))""".stripMargin
}
