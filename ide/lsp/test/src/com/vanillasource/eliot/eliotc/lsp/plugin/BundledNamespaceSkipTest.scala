package com.vanillasource.eliot.eliotc.lsp.plugin

import cats.effect.{IO, Resource}
import cats.effect.testing.scalatest.AsyncIOSpec
import com.vanillasource.eliot.eliotc.apidoc.plugin.ApiDocPlugin
import com.vanillasource.eliot.eliotc.compiler.{CompilationSession, Compiler}
import com.vanillasource.eliot.eliotc.lsp.LspCompileTestLayers
import com.vanillasource.eliot.eliotc.lsp.virtual.VirtualFileSystem
import com.vanillasource.eliot.eliotc.plugin.{Configuration, LangPlugin}
import com.vanillasource.eliot.eliotc.stdlib.plugin.StdlibPlugin
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

/** Regression for the whole-workspace driver enumerating bundled-library files as if they were user modules.
  *
  * The workspace folder an editor hands the server is whatever directory the user opened — for the Eliot compiler repo
  * itself, that is the repo root, a folder that *contains* the layer source roots rather than *being* one. The driver
  * then derives each stdlib file's module name relative to that folder (e.g. `layer.eliot.lang.String` instead of
  * `eliot.lang.String`), which breaks the "don't import yourself" filter in `ModuleValueProcessor`: the mis-rooted file
  * auto-imports the real `eliot.lang.String` and reports `Imported names shadow local names`. Those files are supplied by
  * the bundled layer roots ([[com.vanillasource.eliot.eliotc.lsp.server.BundledLayers]]), so the driver must skip them —
  * they are dependencies, not the user's own code.
  */
class BundledNamespaceSkipTest extends AsyncFlatSpec with AsyncIOSpec with Matchers {
  // Type-checks against the *abstract* platform-independent workspace the LSP compiles (see VirtualFileSystemCompileTest).
  private val userSource   = """def greeting: String = "Hello World!""""
  // A stdlib redefinition placed below the workspace root, so its module name mis-roots to `layer.eliot.lang.String`.
  private val stdlibSource = """type String"""

  "the whole-workspace driver" should "not diagnose bundled-namespace files nested under the workspace root" in {
    withWorkspace { (root, compile) =>
      for {
        errors <- compile
      } yield {
        val _ = errors.filter(_.contains("shadow")) shouldBe empty
        errors shouldBe empty
      }
    }
  }

  /** A workspace root holding a user module (`app/Main.els`) plus a nested source root whose `eliot/lang/String.els`
    * redefines a bundled type — the shape that made the compiler repo light up with shadow errors.
    */
  private def withWorkspace[A](body: (Path, IO[Seq[String]]) => IO[A]): IO[A] =
    tempDirectory.use { root =>
      val userFile   = root.resolve("app").resolve("Main.els")
      val stdlibFile = root.resolve("layer").resolve("eliot").resolve("lang").resolve("String.els")
      val lspPlugin  = LspPlugin(new VirtualFileSystem)
      val config     = LspCompileTestLayers.add(
        Configuration()
          .set(Compiler.targetPathKey, root.resolve(".eliot-lsp"))
          .set(LangPlugin.pathKey, Seq(root))
      )
      for {
        _       <- IO.blocking(Files.createDirectories(userFile.getParent))
        _       <- IO.blocking(Files.createDirectories(stdlibFile.getParent))
        _       <- IO.blocking(Files.writeString(userFile, userSource))
        _       <- IO.blocking(Files.writeString(stdlibFile, stdlibSource))
        session <- CompilationSession.create(
                     lspPlugin,
                     Seq(lspPlugin, LangPlugin(), StdlibPlugin(), ApiDocPlugin()),
                     config,
                     List(root.toString)
                   )
        compile  = session.compileOnce().map(_.errors.map(_.message))
        result  <- body(root, compile)
      } yield result
    }

  private def tempDirectory: Resource[IO, Path] =
    Resource.make(IO.blocking(Files.createTempDirectory("eliot-lsp-namespace")))(dir =>
      IO.blocking {
        Files.walk(dir).sorted(java.util.Comparator.reverseOrder()).iterator().asScala.foreach(Files.delete)
      }
    )
}
