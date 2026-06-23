package com.vanillasource.eliot.eliotc.jvm

import cats.effect.{IO, Resource}
import cats.effect.testing.scalatest.AsyncIOSpec
import com.vanillasource.eliot.eliotc.compiler.Compiler
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, InputStream, PrintStream}
import java.net.URLClassLoader
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

trait FullIntegrationTest extends AsyncFlatSpec with AsyncIOSpec with Matchers {

  /** Compile `source` (as module `moduleName`) to an executable jar and run it, returning its captured standard output.
    * `stdin` is fed to the program's standard input — needed by `Console.readLine`-based programs.
    */
  protected def compileAndRun(source: String, moduleName: String = "Test", stdin: String = ""): IO[String] =
    (for {
      sourceDir <- tempDirectory("eliot-src")
      targetDir <- tempDirectory("eliot-target")
    } yield (sourceDir, targetDir)).use { (sourceDir, targetDir) =>
      for {
        _      <- IO.blocking(Files.writeString(sourceDir.resolve(s"$moduleName.els"), source))
        _      <- Compiler.runCompiler(
                    List("jvm", "exe-jar", sourceDir.toString, "-o", targetDir.toString, "-m", moduleName)
                  )
        output <- runJar(targetDir.resolve(s"$moduleName.jar"), stdin)
      } yield output
    }

  /** Compile `source` and return the compiler's printed diagnostics (captured from standard output). The build is
    * expected to fail; the returned text contains the reported error messages.
    */
  protected def compileForErrors(source: String, moduleName: String = "Test"): IO[String] =
    (for {
      sourceDir <- tempDirectory("eliot-src")
      targetDir <- tempDirectory("eliot-target")
    } yield (sourceDir, targetDir)).use { (sourceDir, targetDir) =>
      for {
        _      <- IO.blocking(Files.writeString(sourceDir.resolve(s"$moduleName.els"), source))
        output <- IO.blocking {
                    val baos        = new ByteArrayOutputStream()
                    val printStream = new PrintStream(baos)
                    val oldOut      = System.out
                    val oldErr      = System.err
                    try {
                      // Compiler diagnostics are written via cats-effect `Console[IO].errorln` (standard error); capture
                      // both streams so the reported error text is observable here.
                      System.setOut(printStream)
                      System.setErr(printStream)
                      Compiler
                        .runCompiler(List("jvm", "exe-jar", sourceDir.toString, "-o", targetDir.toString, "-m", moduleName))
                        .unsafeRunSync()(using cats.effect.unsafe.IORuntime.global)
                    } finally {
                      System.setOut(oldOut)
                      System.setErr(oldErr)
                    }
                    printStream.flush()
                    baos.toString
                  }
      } yield output
    }

  /** Compile `source` to an executable jar, then run it as a separate `java -jar` process for up to `timeoutMillis`
    * before forcibly terminating it, returning whatever it printed to standard output meanwhile. Used to exercise a
    * deliberately non-terminating (`{Inf}`) program: the loop never returns on its own, so the test bounds it by
    * killing the process and inspecting the partial (repeated) output.
    */
  protected def compileAndRunBounded(source: String, timeoutMillis: Long, moduleName: String = "Test"): IO[String] =
    (for {
      sourceDir <- tempDirectory("eliot-src")
      targetDir <- tempDirectory("eliot-target")
    } yield (sourceDir, targetDir)).use { (sourceDir, targetDir) =>
      for {
        _      <- IO.blocking(Files.writeString(sourceDir.resolve(s"$moduleName.els"), source))
        _      <- Compiler.runCompiler(
                    List("jvm", "exe-jar", sourceDir.toString, "-o", targetDir.toString, "-m", moduleName)
                  )
        output <- runJarBounded(targetDir.resolve(s"$moduleName.jar"), timeoutMillis)
      } yield output
    }

  private def runJarBounded(jarPath: Path, timeoutMillis: Long): IO[String] = IO.blocking {
    // Run out-of-process (not via an in-process classloader as `runJar` does): an `{Inf}` program never returns, so it
    // must be killed, which a reflective in-VM call cannot survive. Output is redirected to a file to avoid a pipe
    // deadlock when the unbounded loop outpaces a reader.
    val outFile = Files.createTempFile("eliot-bounded-out", ".txt")
    val javaBin = Path.of(System.getProperty("java.home"), "bin", "java").toString
    val process = new ProcessBuilder(javaBin, "-jar", jarPath.toString)
      .redirectErrorStream(true)
      .redirectOutput(outFile.toFile)
      .start()
    try process.waitFor(timeoutMillis, java.util.concurrent.TimeUnit.MILLISECONDS)
    finally {
      process.destroyForcibly()
      process.waitFor()
    }
    val output = Files.readString(outFile)
    Files.deleteIfExists(outFile)
    output
  }

  private def runJar(jarPath: Path, stdin: String): IO[String] = IO.blocking {
    val classLoader = new URLClassLoader(Array(jarPath.toUri.toURL), ClassLoader.getPlatformClassLoader)
    try {
      val mainClass  = classLoader.loadClass("main")
      val mainMethod = mainClass.getMethod("main", classOf[Array[String]])
      val baos       = new ByteArrayOutputStream()
      val printStream = new PrintStream(baos)
      val inStream    = new ByteArrayInputStream(stdin.getBytes(StandardCharsets.UTF_8))
      val oldOut     = System.out
      val oldIn      = System.in
      try {
        System.setOut(printStream)
        System.setIn(inStream)
        Console.withOut(printStream) {
          mainMethod.invoke(null, Array.empty[String])
        }
      } finally {
        System.setOut(oldOut)
        System.setIn(oldIn)
      }
      printStream.flush()
      baos.toString.stripLineEnd
    } finally {
      classLoader.close()
    }
  }

  private def tempDirectory(prefix: String): Resource[IO, Path] =
    Resource.make(IO.blocking(Files.createTempDirectory(prefix)))(dir =>
      IO.blocking {
        Files.walk(dir).sorted(java.util.Comparator.reverseOrder()).iterator().asScala.foreach(Files.delete)
      }
    )
}
