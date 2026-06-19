package com.vanillasource.eliot.eliotc.lsp.index

import cats.effect.{IO, Resource}
import cats.effect.testing.scalatest.AsyncIOSpec
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.compiler.{CompilationSession, Compiler}
import com.vanillasource.eliot.eliotc.lsp.plugin.LspPlugin
import com.vanillasource.eliot.eliotc.lsp.virtual.VirtualFileSystem
import com.vanillasource.eliot.eliotc.plugin.{Configuration, LangPlugin}
import com.vanillasource.eliot.eliotc.resolve.fact.ResolvedValue
import com.vanillasource.eliot.eliotc.stdlib.plugin.StdlibPlugin
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

import java.net.URI
import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

/** End-to-end proof that a document declaring `main` is recognised as runnable, with the declaring module name the JVM
  * backend's `exe-jar -m` argument expects. The compile runs through a real session (LspPlugin + VFS, LangPlugin,
  * StdlibPlugin; no JVM backend) and the index is built from the materialised [[ResolvedValue]] facts exactly as the
  * service builds it.
  */
class MainIndexCompileTest extends AsyncFlatSpec with AsyncIOSpec with Matchers {
  private val withMain    = """def greeting: IO[Unit] = println("Hello World!")
                            |def main: IO[Unit] = greeting""".stripMargin
  private val withoutMain = """def greeting: IO[Unit] = println("Hello World!")"""

  "main index" should "recognise a document declaring main, carrying its module name" in {
    withCompiledWorkspace(withMain)((uri, index) => index.mainAt(uri).map(_.moduleName.show))
      .asserting(_ shouldBe Some("Test"))
  }

  it should "report no runnable main for a document that declares none" in {
    withCompiledWorkspace(withoutMain)((uri, index) => index.mainAt(uri)).asserting(_ shouldBe None)
  }

  /** Compile a one-file workspace, build the main index from the materialised facts, and hand the test the file's URI
    * alongside the index.
    */
  private def withCompiledWorkspace[A](source: String)(body: (URI, MainIndex) => A): IO[A] =
    tempDirectory.use { sourceDir =>
      val file          = sourceDir.resolve("Test.els")
      val vfs           = new VirtualFileSystem
      val lspPlugin     = LspPlugin(vfs)
      val configuration = Configuration()
        .set(Compiler.targetPathKey, sourceDir.resolve(".eliot-lsp"))
        .set(LangPlugin.pathKey, Seq(sourceDir))
      for {
        _       <- IO.blocking(Files.writeString(file, source))
        session <- CompilationSession.create(
                     lspPlugin,
                     Seq(lspPlugin, LangPlugin(), StdlibPlugin()),
                     configuration,
                     List(sourceDir.toString)
                   )
        result  <- session.compileOnce()
        facts   <- result.generator.currentFacts()
      } yield {
        val resolved = facts.values.collect { case value: ResolvedValue => value }.toSeq
        body(file.toUri, MainIndex.build(resolved))
      }
    }

  private def tempDirectory: Resource[IO, Path] =
    Resource.make(IO.blocking(Files.createTempDirectory("eliot-lsp-main")))(dir =>
      IO.blocking {
        Files.walk(dir).sorted(java.util.Comparator.reverseOrder()).iterator().asScala.foreach(Files.delete)
      }
    )
}
