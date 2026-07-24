package com.vanillasource.eliot.eliotc.jvm

import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import com.vanillasource.eliot.eliotc.compiler.Compiler
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Path}
import java.util.zip.ZipInputStream

/** Effects-as-channel U3a-2b(ii), first wiring slice (docs/effects-as-channel.md §10): the transitional
  * `--uniform-carrier` gate must emit **byte-identical** bytecode to the default path for the shapes it covers. Under the
  * flag a pure value return routes through the uniform boundary
  * ([[com.vanillasource.eliot.eliotc.monomorphize.check.UniformCarrierChecker.checkReturnBoundary]] — `pure@Id`/`runId`
  * inserted), which the downstream Id-normalization stage then erases, so the emitted code must be unchanged.
  *
  * This compiles the same program — which pulls in the whole base layer (`lang` + `stdlib` + `jvm`) — with the flag off
  * and on and asserts every generated class's bytes match, so it validates the uniform boundary across *every* pure value
  * return in the base, not just the program's own. It is the durable successor of the manual `cmp` used to bring the
  * slice up; two full base compiles is the point (the whole base must compile byte-identically under the flag).
  */
class UniformCarrierByteIdenticalTest extends AsyncFlatSpec with AsyncIOSpec with Matchers {

  // Exercises the shapes the uniform gate routes today: a pure value return (`label`'s `line`), a pure argument into a
  // payload slot (`printLine(<pure>)`), and an *effectful* argument into a payload slot (`label(readLine)` — `readLine`
  // is `{Console} String`, bound at the call site). The program plus the whole base layer must compile byte-identically
  // with the flag off vs on.
  private val source =
    """def label(line: String): String = line
      |
      |def main: {Console} Unit = printLine(label(readLine))
      |""".stripMargin

  // Exercises the whole conditional surface (`if`/`else`/`fold` are ordinary functions, never hardcoded): a
  // discharge-to-pure `if..else` whose residual carrier defaults to `Id` and unwraps with `runId` (`sign`), an
  // effectful `if..else` whose `Abort` is discharged by `else` while `Console` rides the ambient (`report`), a
  // multi-arm `fold` (both bare-`A` Generic arms, only the selected one run — `pick`), and a `val`-bound discharged
  // chain (`describe`). Each must compile byte-identically off vs on: the `if`'s pure arm pure-wraps at the concrete
  // `AbortCarrier` carrier (never defaulted to `Id`), the discharger's `computation` slot *captures* the effectful
  // computation (never binds/sequences it), and every inserted `pure@Id`/`runId` erases.
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
  // value — it must be *sequenced* (bound) at the call site, exactly as the default path's Phase-B `tryBindLift` does. The
  // ride-up sibling is covered by `pick`'s `fold` arms in `conditionalSource`; this pins the bind sibling byte-identical.
  private val genericBindSource =
    """def first[A, B](a: A, b: B): A = a
      |
      |def main: {Console} Unit = printLine(first("x", readLine))
      |""".stripMargin

  // Exercises the payload-slot CAPTURE case (U4-a(ii)): an effectful actual captured *whole* into a carrier-stack /
  // pinned domain. `parseOk : {Throw[String]} String` (desugars to a role-carrier `?F[String]`) is passed to `catch`'s
  // `computation: {Throw[E] | G} A` slot (a pinned `ThrowCarrier[E, G, A]`); its payload `String` does not fit the
  // domain, but the whole `?F[String]` pass-through-unifies (`?F := ThrowCarrier[E, G]`, `A := String`), storing the
  // computation — the uniform ladder's arm-1 whole-type pass-through, byte-identical to the default whole-unify.
  private val captureSource =
    """def parseOk: {Throw[String]} String = "parsed-value"
      |
      |def main: {Console} Unit = printLine(parseOk catch (err -> err))
      |""".stripMargin

  // Exercises the doomed under-applied BIND case (U4-a(ii)): a fully-polymorphic effectful actual (`abort : {Abort} ?A`
  // = `?F[?A]`, bare-flex payload) into `printLine`'s nullary `String` domain. The payload does not fit (bare flex) and
  // the whole-type unify is *doomed* (`?F[?A] ~ String` has no injective solution), so the effect must bind-lift: `?A :=
  // String`, the Abort sequences at the call site. `runAbort` discharges it. Byte-identical to the default `tryBindLift`.
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
  // hand-written programs above do not deeply cover. Byte-identical off vs on is verified across the whole example corpus
  // (34/34 mains, 2026-07-24); this pins the most complex shape as a permanent regression guard.
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
      |def demo: Pair[String, String] = runId(swap("second").runStateToPair("first"))
      |
      |def main: {Console} Unit = {
      |   printLine(demo.first)
      |   printLine(demo.second)
      |}
      |""".stripMargin

  // Exercises NESTED effect-carrier stacks (U4-e prerequisite — the `CarrierJoin` prefix-unify fix): `grade`'s
  // `if..else if..else` monomorphizes the `Effect[AbortCarrier[G]]` instance at `AbortCarrier[AbortCarrier[IO]]`, whose
  // inner binder `G` the uniform join must solve (it dropped the `Con` prefix before, leaving `G` unsolved → "contains
  // unresolved variable"). Byte-identical off vs on now that the prefix unifies.
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
      |def main: IO[Unit] = printLine(firstDep.provide(Database("the-db")).provide(Logger("the-logger")))
      |""".stripMargin

  "the --uniform-carrier gate" should "emit byte-identical classes to the default path (whole base + program)" in {
    (for {
      off <- compileClasses(source, uniformCarrier = false)
      on  <- compileClasses(source, uniformCarrier = true)
    } yield (off, on)).asserting { case (off, on) => on shouldBe off }
  }

  it should "emit byte-identical classes for the conditional surface (if/else/fold, discharge-to-pure, capture)" in {
    (for {
      off <- compileClasses(conditionalSource, uniformCarrier = false)
      on  <- compileClasses(conditionalSource, uniformCarrier = true)
    } yield (off, on)).asserting { case (off, on) => on shouldBe off }
  }

  it should "emit byte-identical classes for the Generic-arm bind case (effectful arg into a discarded type-param slot)" in {
    (for {
      off <- compileClasses(genericBindSource, uniformCarrier = false)
      on  <- compileClasses(genericBindSource, uniformCarrier = true)
    } yield (off, on)).asserting { case (off, on) => on shouldBe off }
  }

  it should "emit byte-identical classes for the payload-slot capture case (effectful computation captured by a discharger)" in {
    (for {
      off <- compileClasses(captureSource, uniformCarrier = false)
      on  <- compileClasses(captureSource, uniformCarrier = true)
    } yield (off, on)).asserting { case (off, on) => on shouldBe off }
  }

  it should "emit byte-identical classes for the doomed under-applied bind case (fully-polymorphic effectful actual)" in {
    (for {
      off <- compileClasses(doomedBindSource, uniformCarrier = false)
      on  <- compileClasses(doomedBindSource, uniformCarrier = true)
    } yield (off, on)).asserting { case (off, on) => on shouldBe off }
  }

  it should "emit byte-identical classes for a State transformer-stack program (direct-style, discharged under Id)" in {
    (for {
      off <- compileClasses(stateSource, uniformCarrier = false)
      on  <- compileClasses(stateSource, uniformCarrier = true)
    } yield (off, on)).asserting { case (off, on) => on shouldBe off }
  }

  it should "emit byte-identical classes for a NESTED AbortCarrier stack (if..else if..else at two carrier depths)" in {
    (for {
      off <- compileClasses(nestedAbortSource, uniformCarrier = false)
      on  <- compileClasses(nestedAbortSource, uniformCarrier = true)
    } yield (off, on)).asserting { case (off, on) => on shouldBe off }
  }

  it should "emit byte-identical classes for two distinct-typed nested Dep carriers" in {
    (for {
      off <- compileClasses(twoDepsSource, uniformCarrier = false)
      on  <- compileClasses(twoDepsSource, uniformCarrier = true)
    } yield (off, on)).asserting { case (off, on) => on shouldBe off }
  }

  it should "produce identical errors for a payload-slot mismatch (pure actual not fitting, no capture)" in {
    // `printLine(true)` — `Bool` into the `String` domain — reaches `uniformCaptureSlot`'s mismatch leaf (not doomed, no
    // whole-type capture), which now commits the mismatch directly rather than via `defaultArgSlot`. The reported errors
    // must be identical off vs on (and non-empty, so the check is not vacuous).
    (for {
      off <- compileErrors("def main: {Console} Unit = printLine(true)\n", uniformCarrier = false)
      on  <- compileErrors("def main: {Console} Unit = printLine(true)\n", uniformCarrier = true)
    } yield (off, on)).asserting { case (off, on) => (off.nonEmpty, on) shouldBe (true, off) }
  }

  /** Compile the program (module `Test`) over the base layer roots, optionally under `--uniform-carrier`, and return each
    * generated class's name → bytes. A fresh session per call keeps the two runs independent.
    */
  private def compileClasses(source: String, uniformCarrier: Boolean): IO[Map[String, Seq[Byte]]] =
    for {
      sourceDir  <- IO.blocking(Files.createTempDirectory("eliot-uc-src"))
      targetDir  <- IO.blocking(Files.createTempDirectory("eliot-uc-target"))
      _          <- IO.blocking(Files.writeString(sourceDir.resolve("Test.els"), source))
      flag        = if (uniformCarrier) Nil else List("--legacy-carrier")
      args        = List("jvm", "exe-jar", sourceDir.toString, "-o", targetDir.toString, "-m", "Test") ++
                      layerPathArgs ++ flag
      sessionOpt <- Compiler.createSession(args)
      session    <- IO.fromOption(sessionOpt)(new IllegalStateException("Could not create the compilation session."))
      result     <- session.compileOnce()
      _          <- IO.raiseWhen(result.errors.nonEmpty)(
                      new IllegalStateException(s"Compilation errors: ${result.errors.map(_.message).mkString(", ")}")
                    )
      classes    <- readClasses(targetDir.resolve("Test.jar"))
    } yield classes

  /** Compile the program and return its sorted error messages (never raising) — for a program expected NOT to compile,
    * validating the uniform gate reports the identical errors as the default path.
    */
  private def compileErrors(source: String, uniformCarrier: Boolean): IO[Seq[String]] =
    for {
      sourceDir  <- IO.blocking(Files.createTempDirectory("eliot-uc-src"))
      targetDir  <- IO.blocking(Files.createTempDirectory("eliot-uc-target"))
      _          <- IO.blocking(Files.writeString(sourceDir.resolve("Test.els"), source))
      flag        = if (uniformCarrier) Nil else List("--legacy-carrier")
      args        = List("jvm", "exe-jar", sourceDir.toString, "-o", targetDir.toString, "-m", "Test") ++
                      layerPathArgs ++ flag
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
