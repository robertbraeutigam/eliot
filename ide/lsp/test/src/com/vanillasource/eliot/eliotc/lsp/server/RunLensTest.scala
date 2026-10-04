package com.vanillasource.eliot.eliotc.lsp.server

import com.vanillasource.eliot.eliotc.lsp.index.{MainIndex, TestIndex}
import com.vanillasource.eliot.eliotc.lsp.server.EliotCompilationService.{RunTarget, TestTarget}
import com.vanillasource.eliot.eliotc.module.fact.ModuleName
import com.vanillasource.eliot.eliotc.pos.{Position, PositionRange}
import org.eclipse.lsp4j.CodeLens
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.Path
import scala.jdk.CollectionConverters.*

/** The lenses a document's runnable entries are offered as: what the client sees (title, command) and what it launches
  * from (the arguments), which are `[buildRoot, moduleName, dependencyRoot*]` for both.
  */
class RunLensTest extends AnyFlatSpec with Matchers {
  private val range        = PositionRange(Position(3, 5), Position(3, 14))
  private val module       = ModuleName(Seq("my", "pkg"), "Greeter")
  private val root         = Path.of("/p/test/src")
  private val dependencies = Seq(Path.of("/p/src"), Path.of("/c/eliot/stdlib"))

  "the run tests lens" should "offer the suite's command, titled for what it runs" in {
    val lens = EliotTextDocumentService.runTestsLens(TestTarget(root, TestIndex.Entry(range, module), dependencies))
    (lens.getCommand.getTitle, lens.getCommand.getCommand) shouldBe (("▶ Run tests", "eliot.runTests"))
  }

  it should "carry the build root, the suite's module and then the dependency roots" in {
    val lens = EliotTextDocumentService.runTestsLens(TestTarget(root, TestIndex.Entry(range, module), dependencies))
    argumentsOf(lens) shouldBe List("/p/test/src", "my.pkg.Greeter", "/p/src", "/c/eliot/stdlib")
  }

  it should "sit on the suite's name, in the editor's zero-based positions" in {
    val lens = EliotTextDocumentService.runTestsLens(TestTarget(root, TestIndex.Entry(range, module), dependencies))
    (lens.getRange.getStart.getLine, lens.getRange.getStart.getCharacter, lens.getRange.getEnd.getCharacter) shouldBe ((2, 4, 13))
  }

  "the run main lens" should "keep its command and argument shape" in {
    val lens = EliotTextDocumentService.runMainLens(RunTarget(root, MainIndex.Entry(range, module, true), dependencies))
    (lens.getCommand.getTitle, lens.getCommand.getCommand, argumentsOf(lens)) shouldBe
      (("▶ Run main", "eliot.runMain", List("/p/test/src", "my.pkg.Greeter", "/p/src", "/c/eliot/stdlib")))
  }

  private def argumentsOf(lens: CodeLens): List[Any] = lens.getCommand.getArguments.asScala.toList
}
