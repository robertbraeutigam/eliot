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

  "the --uniform-carrier gate" should "emit byte-identical classes to the default path (whole base + program)" in {
    (for {
      off <- compileClasses(uniformCarrier = false)
      on  <- compileClasses(uniformCarrier = true)
    } yield (off, on)).asserting { case (off, on) => on shouldBe off }
  }

  /** Compile the program (module `Test`) over the base layer roots, optionally under `--uniform-carrier`, and return each
    * generated class's name → bytes. A fresh session per call keeps the two runs independent.
    */
  private def compileClasses(uniformCarrier: Boolean): IO[Map[String, Seq[Byte]]] =
    for {
      sourceDir  <- IO.blocking(Files.createTempDirectory("eliot-uc-src"))
      targetDir  <- IO.blocking(Files.createTempDirectory("eliot-uc-target"))
      _          <- IO.blocking(Files.writeString(sourceDir.resolve("Test.els"), source))
      flag        = if (uniformCarrier) List("--uniform-carrier") else Nil
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
