package com.vanillasource.eliot.eliotc.lsp.server

import cats.effect.{IO, Resource}
import cats.effect.testing.scalatest.AsyncIOSpec
import com.vanillasource.eliot.eliotc.apidoc.plugin.ApiDocPlugin
import com.vanillasource.eliot.eliotc.compiler.{CompilationSession, Compiler}
import com.vanillasource.eliot.eliotc.lsp.plugin.LspPlugin
import com.vanillasource.eliot.eliotc.lsp.virtual.VirtualFileSystem
import com.vanillasource.eliot.eliotc.plugin.{Configuration, LangPlugin}
import com.vanillasource.eliot.eliotc.stdlib.plugin.StdlibPlugin
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

/** End-to-end proof that a standalone, Mill-style project — sources directly under `src/`, no `eliot/` qualifier and no
  * `eliot-compiler/` sibling — compiles cleanly when its only knowledge of the base/stdlib/platform layers is an
  * `eliot.paths` file. The file lists the repo's layer roots (handed in via `ELIOT_REPO_ROOT`, see `build.mill`) as
  * runtime roots alongside the project's own `src`, and the `stdlib` overlay as a separate compiler root; the config
  * flows into `LangPlugin.pathKey` + `LangPlugin.compilerPathKey` exactly as `EliotCompilationService.startWorkspace`
  * wires it. A clean compile means `printLine`/`IO` resolved purely through the paths the file declared.
  */
class WorkspacePathsCompileTest extends AsyncFlatSpec with AsyncIOSpec with Matchers {
  private val repoRoot            =
    Path.of(Option(System.getenv("ELIOT_REPO_ROOT")).getOrElse(System.getProperty("user.dir")))
  private def layer(module: String): Path = repoRoot.resolve(module).resolve("eliot")

  // No `main`: the whole-workspace driver saturates (resolves + type-checks) every declared name, which is what proves
  // the layers resolve. It deliberately does not declare `main` — monomorphizing one in the backend-less LSP compile is
  // "first step only, no error recovery" (see LspPlugin) and emits internal-error facts unrelated to path resolution.
  private val program = """import eliot.jvm.IO
import eliot.effect.Console
                          |def greeting: IO[Unit] = printLine("Hello World!")""".stripMargin

  private val eliotPaths = Seq(
    s"# base + platform layers from the compiler checkout",
    s"runtime  ${layer("lang")}",
    s"runtime  ${layer("stdlib")}",
    s"runtime  ${layer("jvm")}",
    s"# this project's own Mill-style source root",
    s"runtime  src",
    s"# the stdlib compile-time overlay, listed separately",
    s"compiler ${repoRoot.resolve("stdlib").resolve("eliot-compiler")}"
  ).mkString("\n")

  "a Mill-style project configured by eliot.paths" should "compile with no errors, resolving the layers it lists" in {
    tempDirectory
      .use { workspace =>
        val loaded        = WorkspacePaths.load(Seq(workspace)).getOrElse(fail("eliot.paths was not loaded"))
        val lspPlugin     = LspPlugin(new VirtualFileSystem)
        val configuration = Configuration()
          .set(Compiler.targetPathKey, workspace.resolve(".eliot-lsp"))
          .set(LangPlugin.pathKey, loaded.runtimeRoots)
          .set(LangPlugin.compilerPathKey, loaded.compilerRoots)
        for {
          session <- CompilationSession.create(
                       lspPlugin,
                       Seq(lspPlugin, LangPlugin(), StdlibPlugin(), ApiDocPlugin()),
                       configuration,
                       (loaded.runtimeRoots ++ loaded.compilerRoots).map(_.toString).toList
                     )
          result  <- session.compileOnce()
        } yield result.errors
      }
      .asserting(_ shouldBe empty)
  }

  private def tempDirectory: Resource[IO, Path] =
    Resource.make(IO.blocking {
      val workspace = Files.createTempDirectory("eliot-lsp-paths")
      Files.createDirectory(workspace.resolve("src"))
      Files.writeString(workspace.resolve("src").resolve("Main.els"), program)
      Files.writeString(workspace.resolve(WorkspacePaths.fileName), eliotPaths)
      workspace
    })(dir =>
      IO.blocking(Files.walk(dir).sorted(java.util.Comparator.reverseOrder()).iterator().asScala.foreach(Files.delete))
    )
}
