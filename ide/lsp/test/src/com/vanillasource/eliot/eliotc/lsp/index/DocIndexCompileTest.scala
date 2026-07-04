package com.vanillasource.eliot.eliotc.lsp.index

import cats.effect.{IO, Resource}
import cats.effect.testing.scalatest.AsyncIOSpec
import com.vanillasource.eliot.eliotc.apidoc.fact.ValueDoc
import com.vanillasource.eliot.eliotc.apidoc.plugin.ApiDocPlugin
import com.vanillasource.eliot.eliotc.compiler.{CompilationSession, Compiler}
import com.vanillasource.eliot.eliotc.lsp.plugin.LspPlugin
import com.vanillasource.eliot.eliotc.lsp.virtual.VirtualFileSystem
import com.vanillasource.eliot.eliotc.lsp.LspCompileTestLayers
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, QualifiedName, Qualifier, ValueFQN}
import com.vanillasource.eliot.eliotc.plugin.{Configuration, LangPlugin}
import com.vanillasource.eliot.eliotc.stdlib.plugin.StdlibPlugin
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

/** End-to-end proof that the language server surfaces the apidoc documentation of a name. The compile runs through a
  * real session that additionally activates [[ApiDocPlugin]] (processors only — as in the resident service, its HTML
  * `run` never fires since [[LspPlugin]] is the target); [[LspPlugin]] demands a [[ValueDoc]] for every documentable
  * name across the layers, and the index is built from those facts exactly as the service builds it. So a `/** ... */`
  * comment written on a definition becomes the hover documentation for every reference to it.
  */
class DocIndexCompileTest extends AsyncFlatSpec with AsyncIOSpec with Matchers {
  private val source =
    """/** The canonical greeting shown at startup. */
      |def greeting: String = "hello"
      |
      |def other: String = greeting""".stripMargin

  private val module          = ModuleName(Seq.empty, "Test")
  private def fqn(name: String): ValueFQN = ValueFQN(module, QualifiedName(name, Qualifier.Default))

  "doc index" should "carry the doc comment of a documented definition" in {
    tileFor(fqn("greeting")).asserting(_.flatMap(_.doc) shouldBe Some("The canonical greeting shown at startup."))
  }

  it should "carry the rendered definition signature of a documented definition" in {
    tileFor(fqn("greeting")).asserting(_.flatMap(_.signature) shouldBe Some("def greeting: String"))
  }

  it should "carry the signature of an undocumented definition, with no doc" in {
    tileFor(fqn("other")).asserting(_ shouldBe Some(DocIndex.Tile(Some("def other: String"), None)))
  }

  private def tileFor(name: ValueFQN): IO[Option[DocIndex.Tile]] =
    withCompiledWorkspace(index => index.tileFor(name))

  /** Compile a one-file workspace, build the doc index from the materialised [[ValueDoc]] facts, and hand it to the
    * test.
    */
  private def withCompiledWorkspace[A](body: DocIndex => A): IO[A] =
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
                     Seq(lspPlugin, LangPlugin(), StdlibPlugin(), ApiDocPlugin()),
                     configuration,
                     List(sourceDir.toString)
                   )
        result  <- session.compileOnce()
        facts   <- result.generator.currentFacts()
      } yield body(DocIndex.build(facts.values.collect { case value: ValueDoc => value }.toSeq))
    }

  private def tempDirectory: Resource[IO, Path] =
    Resource.make(IO.blocking(Files.createTempDirectory("eliot-lsp-docindex")))(dir =>
      IO.blocking {
        Files.walk(dir).sorted(java.util.Comparator.reverseOrder()).iterator().asScala.foreach(Files.delete)
      }
    )
}
