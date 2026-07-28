package com.vanillasource.eliot.eliotc.jvm

import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import com.vanillasource.eliot.eliotc.compiler.Compiler
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Path}
import java.util.zip.ZipInputStream

/** Effects-as-channel (docs/effects-as-channel.md §0/§10): the uniform-carrier checker — now the sole checker path —
  * must compile the full spread of carrier shapes over the whole base layer (`lang` + `stdlib` + `jvm`). Each program
  * pulls in the base and is compiled end-to-end; a clean compile (classes produced, no errors) is the assertion.
  *
  * This began as a byte-identity oracle comparing the uniform path against the pre-uniform `--legacy-carrier` fallback
  * (the migration methodology). With the flip complete (the uniform path is the live default and the constructor
  * defaults, U4-e close-out slice 2) and `--legacy-carrier` removed, the oracle is retired; the durable value is the
  * program corpus, kept as a uniform-only compile-success regression suite over every shape the uniform ladder covers.
  */
class UniformCarrierCompileTest extends AsyncFlatSpec with AsyncIOSpec with Matchers {

  // Exercises the shapes the uniform gate routes: a pure value return (`label`'s `line`), a pure argument into a payload
  // slot (`printLine(<pure>)`), and an *effectful* argument into a payload slot (`label(readLine)` — `readLine` is
  // `{Console} String`, bound at the call site).
  private val source =
    """def label(line: String): String = line
      |
      |def main: {Console} Unit = printLine(label(readLine))
      |""".stripMargin

  // Exercises the whole conditional surface (`if`/`else`/`fold` are ordinary functions, never hardcoded): a
  // discharge-to-pure `if..else` whose residual carrier defaults to `Id` and unwraps with `runId` (`sign`), an
  // effectful `if..else` whose `Abort` is discharged by `else` while `Console` rides the ambient (`report`), a
  // multi-arm `fold` (both bare-`A` Generic arms, only the selected one run — `pick`), and a `val`-bound discharged
  // chain (`describe`).
  private val conditionalSource =
    """def sign(flag: Bool): String = if(flag, "+") else "-"
      |
      |def describe(a: Bool, b: Bool): String = {
      |   val category = if(a, "first") else if(b, "second") else "third"
      |   category
      |}
      |
      |def report(flag: Bool): {Console} Unit = if(flag, printLine("on")) else printLine("off")
      |
      |def pick(flag: Bool): {Console} Unit = fold(flag, printLine("a"), printLine("b"))
      |
      |def main: {Console} Unit = {
      |   printLine(sign(readLine == "yes"))
      |   printLine(describe(readLine == "a", readLine == "b"))
      |   report(readLine == "y")
      |   pick(readLine == "z")
      |}
      |""".stripMargin

  // Exercises the Generic-arm BIND case (U4-a(i)): a generic callee whose type parameter is *discarded* from the result
  // (`first[A, B](a: A, b: B): A` — `B` absent from `A`) receiving an *effectful* argument in the discarded slot
  // (`first("x", readLine)`). The domain meta `B` does not ride the result, so the effect cannot ride up as a first-class
  // value — it must be *sequenced* (bound) at the call site, exactly as the Phase-B `tryBindLift` does.
  private val genericBindSource =
    """def first[A, B](a: A, b: B): A = a
      |
      |def main: {Console} Unit = printLine(first("x", readLine))
      |""".stripMargin

  // Exercises the payload-slot CAPTURE case (U4-a(ii)): an effectful actual captured *whole* into a carrier-stack /
  // pinned domain. `parseOk : {Throw[String]} String` (desugars to a role-carrier `?F[String]`) is passed to `catch`'s
  // `computation: {Throw[E] | G} A` slot (a pinned `ThrowCarrier[E, G, A]`); its payload `String` does not fit the
  // domain, but the whole `?F[String]` pass-through-unifies (`?F := ThrowCarrier[E, G]`, `A := String`), storing the
  // computation — the uniform ladder's arm-1 whole-type pass-through.
  private val captureSource =
    """def parseOk: {Throw[String]} String = "parsed-value"
      |
      |def main: {Console} Unit = printLine(parseOk catch (err -> err))
      |""".stripMargin

  // Exercises the doomed under-applied BIND case (U4-a(ii)): a fully-polymorphic effectful actual (`abort : {Abort} ?A`
  // = `?F[?A]`, bare-flex payload) into `printLine`'s nullary `String` domain. The payload does not fit (bare flex) and
  // the whole-type unify is *doomed* (`?F[?A] ~ String` has no injective solution), so the effect must bind-lift: `?A :=
  // String`, the Abort sequences at the call site. `runAbort` discharges it.
  private val doomedBindSource =
    """import eliot.jvm.IO
      |import eliot.effect.Console
      |import eliot.carrier.Effect
      |import eliot.effect.Abort
      |
      |def demo: {Abort, Console} Unit = printLine(abort)
      |
      |def main: IO[Unit] = flatMap(o -> printLine(foldOption("done", s -> "got", o)), runAbort(demo))
      |""".stripMargin

  // A rich effect-transformer-stack program (the `EffectsState` example, inlined): a `{State[String]}` computation in
  // direct style (`val old = state; putState(next); old`), discharged under the pure `Id` carrier via `runStateToPair` +
  // `runId`. Exercises the uniform carrier-slot / bind / discharge surface over a real transformer stack — the shape the
  // hand-written programs above do not deeply cover.
  private val stateSource =
    """import eliot.carrier.Effect
      |
      |data Id[A](runId: A)
      |
      |implement Effect[Id] {
      |   def pure[A](a: A): Id[A] = Id(a)
      |   def flatMap[A, B](f: Function[A, Id[B]], fa: Id[A]): Id[B] = f(runId(fa))
      |   def map[A, B](f: Function[A, B], fa: Id[A]): Id[B] = Id(f(runId(fa)))
      |}
      |
      |def swap(next: String): {State[String]} String = {
      |   val old = state
      |   putState(next)
      |   old
      |}
      |
      |def demo: Pair[String, String] = runId(runStateToPair("first", swap("second")))
      |
      |def main: {Console} Unit = {
      |   printLine(demo.first)
      |   printLine(demo.second)
      |}
      |""".stripMargin

  // Exercises NESTED effect-carrier stacks (the `CarrierJoin` prefix-unify fix): `grade`'s `if..else if..else`
  // monomorphizes the `Effect[AbortCarrier[G]]` instance at `AbortCarrier[AbortCarrier[IO]]`, whose inner binder `G` the
  // uniform join must solve (it dropped the `Con` prefix before, leaving `G` unsolved → "contains unresolved variable").
  private val nestedAbortSource =
    """import eliot.jvm.IO
      |import eliot.effect.Console
      |import eliot.effect.Abort
      |
      |def grade(s: String): {Abort} String = if(s == "A", "excellent") else if(s == "B", "good") else "fail"
      |
      |def main: IO[Unit] = {
      |   printLine(grade("A") else "?")
      |   printLine(if(true, "taken") else "skipped")
      |}
      |""".stripMargin

  // Exercises two distinct-typed nested `Dep` carriers (`DepCarrier[Database, DepCarrier[Logger, IO]]`): the second
  // dep's lift instance (`Dep[X2, DepCarrier[X1, G]] where X1 != X2`) resolves only if the uniform join solves the inner
  // carrier prefix — before the fix it reported "No ability implementation found for ability 'Dep' with type arguments
  // [Logger]".
  private val twoDepsSource =
    """import eliot.jvm.IO
      |import eliot.effect.Console
      |import eliot.effect.Dep
      |import eliot.carrier.Effect
      |
      |data Database(url: String)
      |data Logger(name: String)
      |
      |def firstDep: {Dep[Database], Dep[Logger]} String = pick(url(dependency), name(dependency))
      |def pick(a: String, b: String): String = a
      |
      |def main: IO[Unit] = printLine(provide(Logger("the-logger"), provide(Database("the-db"), firstDep)))
      |""".stripMargin

  "the uniform-carrier checker" should "compile a pure value return + payload slots over the whole base" in {
    compileClasses(source).asserting(_ should not be empty)
  }

  it should "compile the conditional surface (if/else/fold, discharge-to-pure, capture)" in {
    compileClasses(conditionalSource).asserting(_ should not be empty)
  }

  it should "compile the Generic-arm bind case (effectful arg into a discarded type-param slot)" in {
    compileClasses(genericBindSource).asserting(_ should not be empty)
  }

  it should "compile the payload-slot capture case (effectful computation captured by a discharger)" in {
    compileClasses(captureSource).asserting(_ should not be empty)
  }

  it should "compile the doomed under-applied bind case (fully-polymorphic effectful actual)" in {
    compileClasses(doomedBindSource).asserting(_ should not be empty)
  }

  it should "compile a State transformer-stack program (direct-style, discharged under Id)" in {
    compileClasses(stateSource).asserting(_ should not be empty)
  }

  it should "compile a NESTED AbortCarrier stack (if..else if..else at two carrier depths)" in {
    compileClasses(nestedAbortSource).asserting(_ should not be empty)
  }

  it should "compile two distinct-typed nested Dep carriers" in {
    compileClasses(twoDepsSource).asserting(_ should not be empty)
  }

  it should "report a payload-slot mismatch (pure actual not fitting, no capture)" in {
    // `printLine(true)` — `Bool` into the `String` domain — reaches `uniformCaptureSlot`'s mismatch leaf (not doomed, no
    // whole-type capture), which commits the mismatch directly. The reported errors must be non-empty.
    compileErrors("def main: {Console} Unit = printLine(true)\n").asserting(_ should not be empty)
  }

  /** Compile the program (module `Test`) over the base layer roots and return each generated class's name → bytes. */
  private def compileClasses(source: String): IO[Map[String, Seq[Byte]]] =
    for {
      sourceDir  <- IO.blocking(Files.createTempDirectory("eliot-uc-src"))
      targetDir  <- IO.blocking(Files.createTempDirectory("eliot-uc-target"))
      _          <- IO.blocking(Files.writeString(sourceDir.resolve("Test.els"), source))
      args        = List("jvm", "exe-jar", sourceDir.toString, "-o", targetDir.toString, "-m", "Test") ++ layerPathArgs
      sessionOpt <- Compiler.createSession(args)
      session    <- IO.fromOption(sessionOpt)(new IllegalStateException("Could not create the compilation session."))
      result     <- session.compileOnce()
      _          <- IO.raiseWhen(result.errors.nonEmpty)(
                      new IllegalStateException(s"Compilation errors: ${result.errors.map(_.message).mkString(", ")}")
                    )
      classes    <- readClasses(targetDir.resolve("Test.jar"))
    } yield classes

  /** Compile the program and return its sorted error messages (never raising) — for a program expected NOT to compile. */
  private def compileErrors(source: String): IO[Seq[String]] =
    for {
      sourceDir  <- IO.blocking(Files.createTempDirectory("eliot-uc-src"))
      targetDir  <- IO.blocking(Files.createTempDirectory("eliot-uc-target"))
      _          <- IO.blocking(Files.writeString(sourceDir.resolve("Test.els"), source))
      args        = List("jvm", "exe-jar", sourceDir.toString, "-o", targetDir.toString, "-m", "Test") ++ layerPathArgs
      sessionOpt <- Compiler.createSession(args)
      session    <- IO.fromOption(sessionOpt)(new IllegalStateException("Could not create the compilation session."))
      result     <- session.compileOnce()
    } yield result.errors.map(_.message).sorted

  private def readClasses(jar: Path): IO[Map[String, Seq[Byte]]] = IO.blocking {
    val in = new ZipInputStream(Files.newInputStream(jar))
    try
      Iterator
        .continually(in.getNextEntry)
        .takeWhile(_ != null)
        .filter(_.getName.endsWith(".class"))
        .map(entry => entry.getName -> in.readAllBytes().toSeq)
        .toMap
    finally in.close()
  }

  /** The base-layer `eliot/` source roots handed to the compiler as filesystem roots (CP1.5), exactly as
    * [[FullIntegrationTest]] does — the repo root is passed to the forked test JVM via `ELIOT_REPO_ROOT`.
    */
  private def layerPathArgs: List[String] = {
    val repoRoot             =
      Path.of(Option(System.getenv("ELIOT_REPO_ROOT")).getOrElse(System.getProperty("user.dir")))
    def root(module: String) = repoRoot.resolve(module).resolve("eliot").toString
    List("--path", root("lang"), "--path", root("stdlib"), "--path", root("jvm"))
  }
}
