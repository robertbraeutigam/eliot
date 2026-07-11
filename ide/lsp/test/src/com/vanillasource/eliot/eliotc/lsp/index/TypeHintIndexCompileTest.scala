package com.vanillasource.eliot.eliotc.lsp.index

import cats.effect.{IO, Resource}
import cats.effect.testing.scalatest.AsyncIOSpec
import com.vanillasource.eliot.eliotc.compiler.{CompilationSession, Compiler}
import com.vanillasource.eliot.eliotc.lsp.plugin.LspPlugin
import com.vanillasource.eliot.eliotc.lsp.virtual.VirtualFileSystem
import com.vanillasource.eliot.eliotc.lsp.LspCompileTestLayers
import com.vanillasource.eliot.eliotc.monomorphize.channel.RefinementTable
import com.vanillasource.eliot.eliotc.monomorphize.fact.MonomorphicValue
import com.vanillasource.eliot.eliotc.plugin.{Configuration, LangPlugin}
import com.vanillasource.eliot.eliotc.pos.Position
import com.vanillasource.eliot.eliotc.stdlib.plugin.StdlibPlugin
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

import java.net.URI
import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

/** End-to-end proof that hovering reports the *concrete monomorphic* type of an expression node, driven by the same
  * per-file `main` monomorphization the service uses. The compile runs through a real session (LspPlugin + VFS,
  * LangPlugin, StdlibPlugin; no JVM backend); [[LspPlugin]] demands `UsedNames(main)`, which forces a
  * [[MonomorphicValue]] for every reachable instantiation, and the index is built from those facts exactly as the
  * service builds it. `greeting`'s body exercises a string literal and a call; `main`'s body exercises a whole-value
  * reference whose type is the value's signature.
  */
class TypeHintIndexCompileTest extends AsyncFlatSpec with AsyncIOSpec with Matchers {
  private val imports = """import eliot.effect.Console"""
  private val line1   = """def greeting: IO[Unit] = printLine("Hello World!")"""
  private val line2   = """def main: IO[Unit] = greeting"""
  private val source  = s"$imports\n$line1\n$line2"

  // `printLine` is import-required (`Console` is in `eliot.effect`, not auto-imported), so the import on line 1 pushes the
  // two defs to lines 2 and 3.
  private val stringPosition   = Position(2, line1.indexOf("Hello") + 1)   // inside the "Hello World!" literal
  private val printLinePosition  = Position(2, line1.indexOf("printLine") + 4) // well inside the `printLine` reference
  private val greetingPosition = Position(3, line2.indexOf("greeting") + 2) // inside the `greeting` reference in `main`
  private val keywordPosition  = Position(2, 1)                             // the `def` keyword — no expression node

  "type hints" should "report the concrete type of a string literal" in {
    renderedTypesAt(stringPosition).asserting(_ shouldBe Seq("String"))
  }

  it should "report the monomorphic function type at a value reference" in {
    renderedTypesAt(printLinePosition).asserting(_ shouldBe Seq("String -> IO[Unit]"))
  }

  it should "report a whole value's type at a reference to it" in {
    renderedTypesAt(greetingPosition).asserting(_ shouldBe Seq("IO[Unit]"))
  }

  it should "report nothing where there is no expression node" in {
    typesAt(keywordPosition).asserting(_ shouldBe None)
  }

  private def renderedTypesAt(position: Position): IO[Seq[String]] =
    typesAt(position).map(_.fold(Seq.empty[String])(_._2.map(GroundValueRenderer.render)))

  private def typesAt(position: Position) =
    withCompiledWorkspace((uri, index) => index.typeHintsAt(uri, position))

  /** Compile a one-file workspace, build the type-hint index from the materialised facts, and hand the test the file's
    * URI alongside the index.
    */
  private def withCompiledWorkspace[A](body: (URI, TypeHintIndex) => A): IO[A] =
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
                     configuration,
                     List(sourceDir.toString)
                   )
        result  <- session.compileOnce()
        facts   <- result.generator.currentFacts()
      } yield {
        val monomorphic = facts.values.collect { case value: MonomorphicValue => value }.toSeq
        val refinements = facts.values.collect { case value: RefinementTable => value }.toSeq
        body(file.toUri, TypeHintIndex.build(monomorphic, refinements))
      }
    }

  private def tempDirectory: Resource[IO, Path] =
    Resource.make(IO.blocking(Files.createTempDirectory("eliot-lsp-typehint")))(dir =>
      IO.blocking {
        Files.walk(dir).sorted(java.util.Comparator.reverseOrder()).iterator().asScala.foreach(Files.delete)
      }
    )
}
