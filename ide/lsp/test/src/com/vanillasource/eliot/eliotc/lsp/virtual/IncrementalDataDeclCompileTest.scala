package com.vanillasource.eliot.eliotc.lsp.virtual

import cats.effect.{IO, Resource}
import cats.effect.testing.scalatest.AsyncIOSpec
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.apidoc.plugin.ApiDocPlugin
import com.vanillasource.eliot.eliotc.compiler.{CompilationSession, Compiler}
import com.vanillasource.eliot.eliotc.lsp.LspCompileTestLayers
import com.vanillasource.eliot.eliotc.lsp.plugin.LspPlugin
import com.vanillasource.eliot.eliotc.plugin.{Configuration, LangPlugin}
import com.vanillasource.eliot.eliotc.stdlib.plugin.StdlibPlugin
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

import java.net.URI
import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

/** Regression guard for the transient "Could not find 'handleCases^PatternMatch#0'" diagnostic that used to appear
  * against a file containing a `data` declaration in the resident LSP session.
  *
  * A `data` declaration synthesizes the pattern-match dispatch method (`handleCases`), whose name enters the module's
  * name set. On the first compile after a file was opened in the editor, its `Sourced` URIs flip `file:`→`vfs:` (the
  * buffer overlay), but the incremental cache still held `file:`-tagged names from the pre-open disk compile. Because
  * `Sourced` identity used to include the URI scheme, a `file:`-keyed demand compared unequal to the same name
  * recomputed under `vfs:`, so `UnifiedModuleValueProcessor`'s `ModuleNames` membership test spuriously missed it and
  * aborted — even though the name was present. The fix makes `Sourced` identity ignore the URI scheme (compare by
  * scheme-specific-part), so the overlay is transparent to name identity.
  *
  * Each test drives the exact incremental path the server uses — build a session once and recompile across edits with
  * the previous run's cache as the next run's `prior`, or persist to disk and re-seed a fresh session. The bug was a
  * concurrency-sensitive flake (~1 in 5), so the boundary-crossing scenarios are looped to catch a regression reliably.
  */
class IncrementalDataDeclCompileTest extends AsyncFlatSpec with AsyncIOSpec with Matchers {
  // Each source declares a `main` that projects a `data` field, so `LspPlugin` monomorphizes `main` and forces the
  // synthesized `handleCases` (the field accessor is a match expression) through the deep saturate/monomorphize path —
  // the value-less-`SemValue` facts that a warm restart or incremental recompile validates by structural drill.
  private val noData       = """def main: String = "Hello""""
  private val withData     = """data Database(url: String)
                               |def main: String = Database("Hello").url""".stripMargin
  private val withData2    = """data Database(url: String)
                               |def main: String = Database("Goodbye").url""".stripMargin
  private val renamedField = """data Database(host: String)
                               |def main: String = Database("Hello").host""".stripMargin
  private val broken       = """data Database(url: String
                               |def main: String = Database("Hello").url""".stripMargin

  private def noHandleCasesError(errors: Seq[String]): org.scalatest.Assertion =
    withClue(s"unexpected diagnostics: ${errors.mkString(" | ")}\n") {
      errors.filter(_.contains("handleCases")) shouldBe empty
    }

  "adding a data declaration incrementally" should "not spuriously fail to find its synthesized handleCases" in {
    withSession(noData) { (vfs, uri, compile) =>
      for {
        cold        <- compile
        _            = vfs.update(uri, withData)
        incremental <- compile
      } yield {
        val _ = noHandleCasesError(cold)
        noHandleCasesError(incremental)
      }
    }
  }

  "editing an unrelated body with a data declaration present" should "keep finding handleCases" in {
    // The scenario that flaked (~1 in 5) before the fix — a body edit that leaves the name set unchanged, forcing a
    // recompile across the file:→vfs: boundary. Looped so a reintroduced scheme-in-identity regression is caught.
    (1 to 15).toList
      .traverse { _ =>
        withSession(withData) { (vfs, uri, compile) =>
          for {
            cold        <- compile
            _            = vfs.update(uri, withData2)
            incremental <- compile
          } yield {
            val _ = noHandleCasesError(cold)
            noHandleCasesError(incremental)
          }
        }
      }
      .map(_.last)
  }

  "renaming a data field incrementally" should "keep finding handleCases" in {
    withSession(withData) { (vfs, uri, compile) =>
      for {
        _           <- compile
        _            = vfs.update(uri, renamedField)
        incremental <- compile
      } yield noHandleCasesError(incremental)
    }
  }

  "recovering from a broken buffer with a data declaration" should "keep finding handleCases" in {
    withSession(withData) { (vfs, uri, compile) =>
      for {
        _         <- compile
        _          = vfs.update(uri, broken)
        _         <- compile
        _          = vfs.update(uri, withData)
        recovered <- compile
      } yield noHandleCasesError(recovered)
    }
  }

  "reapplying identical data content repeatedly" should "keep finding handleCases across every recompile" in {
    withSession(withData) { (vfs, uri, compile) =>
      (1 to 8).toList
        .traverse { _ => IO(vfs.update(uri, withData)) >> compile }
        .map(_.foreach(noHandleCasesError))
    }
  }

  "typing a data declaration token by token" should "never leave a dangling handleCases demand" in {
    val steps = Seq(
      "def greeting: String = \"Hello\"",
      "data\ndef greeting: String = \"Hello\"",
      "data Database\ndef greeting: String = \"Hello\"",
      "data Database(url: String\ndef greeting: String = \"Hello\"",
      withData
    )
    withSession(noData) { (vfs, uri, compile) =>
      steps.toList.traverse { step => IO(vfs.update(uri, step)) >> compile }.map(_.foreach(noHandleCasesError))
    }
  }

  "a warm restart re-seeded from the serialized on-disk cache" should "keep finding handleCases" in {
    tempDirectory.use { sourceDir =>
      val file = sourceDir.resolve("Test.els")
      for {
        _       <- IO.blocking(Files.writeString(file, withData))
        first   <- newSession(sourceDir).use(s => s.compileOnce().map(_.errors.map(_.message)) <* s.persist())
        // A brand-new session seeds its in-memory cache from the just-persisted (value-less for SemValue) disk cache.
        second  <- newSession(sourceDir).use(_.compileOnce().map(_.errors.map(_.message)))
      } yield {
        val _ = noHandleCasesError(first)
        noHandleCasesError(second)
      }
    }
  }

  "many warm restarts accumulating a persisted cache" should "keep finding handleCases on every restart" in {
    tempDirectory.use { sourceDir =>
      val file = sourceDir.resolve("Test.els")
      IO.blocking(Files.writeString(file, withData)) >>
        (1 to 20).toList
          .traverse { _ => newSession(sourceDir).use(s => s.compileOnce().map(_.errors.map(_.message)) <* s.persist()) }
          .map(_.foreach(noHandleCasesError))
    }
  }

  "concurrent compiles racing on one session" should "keep finding handleCases under contention" in {
    withSession(withData) { (_, _, compile) =>
      (1 to 16).toList.parTraverse(_ => compile).map(_.foreach(noHandleCasesError))
    }
  }

  /** Build a session exactly as `EliotCompilationService.startWorkspace` does (LspPlugin + VFS, LangPlugin, StdlibPlugin,
    * ApiDocPlugin; no JVM backend) over `sourceDir`. A fresh VFS is created per session, so a warm restart reads the disk
    * file unless the caller re-populates the overlay.
    */
  private def newSession(sourceDir: Path): Resource[IO, CompilationSession] = {
    val vfs           = new VirtualFileSystem
    val lspPlugin     = LspPlugin(vfs)
    val configuration = LspCompileTestLayers.add(
      Configuration()
        .set(Compiler.targetPathKey, sourceDir.resolve(".eliot-lsp"))
        .set(LangPlugin.pathKey, Seq(sourceDir))
    )
    Resource.eval(
      CompilationSession.create(
        lspPlugin,
        Seq(lspPlugin, LangPlugin(), StdlibPlugin(), ApiDocPlugin()),
        configuration,
        List(sourceDir.toString)
      )
    )
  }

  /** Seed `Test.els` with `initialDiskSource` and hand the test the overlay, the file's URI, and a `compile` action
    * running one incremental compilation on a single resident session and returning its error messages.
    */
  private def withSession[A](initialDiskSource: String)(
      body: (VirtualFileSystem, URI, IO[Seq[String]]) => IO[A]
  ): IO[A] =
    tempDirectory.use { sourceDir =>
      val file          = sourceDir.resolve("Test.els")
      val vfs           = new VirtualFileSystem
      val lspPlugin     = LspPlugin(vfs)
      val configuration = LspCompileTestLayers.add(
        Configuration()
          .set(Compiler.targetPathKey, sourceDir.resolve(".eliot-lsp"))
          .set(LangPlugin.pathKey, Seq(sourceDir))
      )
      for {
        _       <- IO.blocking(Files.writeString(file, initialDiskSource))
        session <- CompilationSession.create(
                     lspPlugin,
                     Seq(lspPlugin, LangPlugin(), StdlibPlugin(), ApiDocPlugin()),
                     configuration,
                     List(sourceDir.toString)
                   )
        compile  = session.compileOnce().map(_.errors.map(_.message))
        result  <- body(vfs, file.toUri, compile)
      } yield result
    }

  private def tempDirectory: Resource[IO, Path] =
    Resource.make(IO.blocking(Files.createTempDirectory("eliot-lsp-incr")))(dir =>
      IO.blocking {
        Files.walk(dir).sorted(java.util.Comparator.reverseOrder()).iterator().asScala.foreach(Files.delete)
      }
    )
}
