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

  /** An effect performed while **building** an argument that a carrier-codomain slot receives — §1 rule 1, where the
    * slot's declared type is an arrow and therefore a rowless *value* position, the row belonging to what applying that
    * function later does.
    *
    * `labelOf` is the shape as the user meets it: `s.take(s.indexOf("="))` is `.(s, take(indexOf("=", s)))`, so the
    * `{Abort}` of `indexOf` is performed while constructing the argument handed to `.`'s `f: A => {Effect} B`. It was
    * rejected outright ("No ability implementation found for ability 'Abort' with type arguments [Id]", reported inside
    * `stdlib`'s `Bool.els`, a file the user never wrote), which is why the equivalent plain call `take(indexOf("=", s),
    * s)` was the only spelling that compiled.
    *
    * The `foreach` lines pin **where** the effect runs, which is the half a verdict-only fix gets wrong: `readLine` is
    * written once, outside the traversal, so it must run once and its one line must prefix all three elements. An
    * elaboration that defers the construction under the invented eta binder instead reads per element — the same
    * program, silently mis-sequenced. The pure-construction line beside it is the control: it must keep working
    * unchanged, since nothing there has to be hoisted.
    */
  val argumentConstructionProgram: String =
    """import eliot.effect.Abort
      |import eliot.collection.List
      |
      |def labelOf(s: String): {Abort} String = s.take(s.indexOf("="))
      |
      |def three: List[String] = append(append(append(empty, "a"), "b"), "c")
      |
      |def tag(mark: String, item: String): {Console} Unit = printLine(mark ++ item)
      |
      |def main: {Console} Unit = {
      |   printLine(labelOf("host=1") else "none")
      |   printLine(labelOf("nokey") else "none")
      |   three.foreach(tag(readLine))
      |   three.foreach(tag("pure:"))
      |}""".stripMargin

  /** Repeated `readLine`: the `Console` effect must be able to read more than one line.
    *
    * A `BufferedReader` reads ahead, so a reader built per call takes the whole of standard input into its buffer and
    * discards it with itself — which is what this native used to do, leaving every read after the first at
    * end-of-stream. The single shared reader is what makes the second and third lines arrive. The leading `printLine`
    * is part of the test, not scenery: the reader is a field on the same generated class, so a print is what triggers
    * that class's initialisation, and a reader captured there rather than at the first read would bind standard input
    * too early.
    */
  val stdinProgram: String =
    """def main: {Console} Unit = {
      |   printLine("start")
      |   printLine("1:" ++ readLine)
      |   printLine("2:" ++ readLine)
      |   printLine("3:" ++ readLine)
      |}""".stripMargin

  /** A deliberately non-terminating program: `Inf` is an ordinary row entry riding the same union. Compile-only. */
  val infProgram: String =
    """import eliot.jvm.IO
      |import eliot.effect.Console
      |import eliot.effect.Inf
      |
      |def main: {Inf, Console} Unit = forever(printLine("tick"))""".stripMargin
}
