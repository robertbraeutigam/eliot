package com.vanillasource.eliot.eliotc.lsp.index

import cats.effect.{IO, Resource}
import cats.effect.testing.scalatest.AsyncIOSpec
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.compiler.{CompilationSession, Compiler}
import com.vanillasource.eliot.eliotc.lsp.plugin.LspPlugin
import com.vanillasource.eliot.eliotc.lsp.virtual.VirtualFileSystem
import com.vanillasource.eliot.eliotc.lsp.LspCompileTestLayers
import com.vanillasource.eliot.eliotc.plugin.{Configuration, LangPlugin}
import com.vanillasource.eliot.eliotc.resolve.fact.ResolvedValue
import com.vanillasource.eliot.eliotc.stdlib.plugin.StdlibPlugin
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

import java.net.URI
import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

/** End-to-end proof that a document declaring `testCases` is recognised as a suite, with the declaring module name that
  * selects it when the runner is started. The compile runs through a real session (LspPlugin + VFS, LangPlugin,
  * StdlibPlugin; no JVM backend) and the index is built from the materialised [[ResolvedValue]] facts exactly as the
  * service builds it.
  */
class TestIndexCompileTest extends AsyncFlatSpec with AsyncIOSpec with Matchers {
  private val withSuite    = """def helper: String = "helper"
                             |def testCases: String = helper""".stripMargin
  private val withoutSuite = """def helper: String = "helper"
                             |def testCase: String = helper""".stripMargin

  "test index" should "recognise a document declaring testCases, carrying its module name" in {
    withCompiledWorkspace(withSuite)((uri, index) => index.testsAt(uri).map(_.moduleName.show)).asserting(_ shouldBe Some("Test"))
  }

  it should "anchor the suite at the name of testCases" in {
    withCompiledWorkspace(withSuite)((uri, index) => index.testsAt(uri).map(_.range.from.line)).asserting(_ shouldBe Some(2))
  }

  it should "report no suite for a document that declares none" in {
    withCompiledWorkspace(withoutSuite)((uri, index) => index.testsAt(uri)).asserting(_ shouldBe None)
  }

  it should "still recognise the suite after an incremental recompile that changed nothing" in {
    withCompiledWorkspace(withSuite, compiles = 2)((uri, index) => index.testsAt(uri).map(_.moduleName.show))
      .asserting(_ shouldBe Some("Test"))
  }

  /** Compile a one-file workspace `compiles` times in one session (every run after the first is incremental), build the
    * test index from the facts the last run built or proved unchanged, and hand the test the file's URI alongside the
    * index.
    */
  private def withCompiledWorkspace[A](source: String, compiles: Int = 1)(body: (URI, TestIndex) => A): IO[A] =
    tempDirectory.use { sourceDir =>
      val file          = sourceDir.resolve("Test.els")
      val lspPlugin     = LspPlugin(new VirtualFileSystem)
      val configuration = LspCompileTestLayers.add(
        Configuration()
          .set(Compiler.targetPathKey, sourceDir.resolve(".eliot-lsp"))
          .set(LangPlugin.pathKey, Seq(sourceDir))
      )
      for {
        _       <- IO.blocking(Files.writeString(file, source))
        session <- CompilationSession.create(
                     lspPlugin,
                     Seq(lspPlugin, LangPlugin(), StdlibPlugin()),
                     configuration
                   )
        result  <- session.compileOnce().replicateA(compiles).map(_.last)
        facts   <- result.generator.currentFactsIncludingUnchanged(_.isInstanceOf[ResolvedValue.Key])
      } yield {
        val resolved = facts.values.collect { case value: ResolvedValue => value }.toSeq
        body(file.toUri, TestIndex.build(resolved))
      }
    }

  private def tempDirectory: Resource[IO, Path] =
    Resource.make(IO.blocking(Files.createTempDirectory("eliot-lsp-test")))(dir =>
      IO.blocking {
        Files.walk(dir).sorted(java.util.Comparator.reverseOrder()).iterator().asScala.foreach(Files.delete)
      }
    )
}
