package com.vanillasource.eliot.intellij.build

import com.vanillasource.eliot.intellij.build.EliotBuildOutputParser.Problem
import com.vanillasource.eliot.intellij.build.EliotBuildOutputParser.Progress
import org.junit.Assert.assertEquals
import org.junit.Test

class EliotBuildOutputParserTest {
  private val esc = "\u001B"

  /** The compiler's report of an undefined name, exactly as it prints it: coloured header, then the snippet. */
  private val error = listOf(
    "$esc[1m/p/src/Broken.els$esc[0m:$esc[1m$esc[31merror$esc[0m:1:38:Name not defined.",
    "$esc[35m  | ",
    "$esc[35m1 |$esc[0m def main: {Console} Unit = printLine($esc[1m$esc[31mundefinedName$esc[0m)",
    "$esc[35m  |                                      $esc[1m$esc[31m^^^^^^^^^^^^^$esc[0m",
  )

  private val problem = Problem(
    "/p/src/Broken.els", 1, 38, "Name not defined.",
    "/p/src/Broken.els:error:1:38:Name not defined.\n  | \n1 | def main: {Console} Unit = printLine(undefinedName)\n  |                                      ^^^^^^^^^^^^^",
  )

  private fun parse(lines: List<String>): List<EliotBuildOutputParser.Event> =
    EliotBuildOutputParser().let { parser -> lines.flatMap(parser::accept) + parser.finish() }

  @Test
  fun readsAnErrorWithItsSnippetAsDetail() {
    assertEquals(listOf(problem), parse(error))
  }

  @Test
  fun dropsARepeatedError() {
    assertEquals(listOf(problem), parse(error + error))
  }

  @Test
  fun endsAnErrorAtTheFirstLineThatIsNotPartOfIt() {
    assertEquals(listOf(problem, Progress("failed 1 error · 1.4s", null, null)), parse(error + "22:25:23 failed 1 error · 1.4s"))
  }

  @Test
  fun readsAProgressLineWithoutATotal() {
    assertEquals(listOf(Progress("parsing     src/Main.els        1.8s", 377, null)), parse(listOf("22:24:38 [   377 facts ] parsing     src/Main.els        1.8s")))
  }

  @Test
  fun readsAProgressLineCountingAgainstATotal() {
    assertEquals(listOf(Progress("checking    src/Main.els", 412, 1327)), parse(listOf("22:24:38 [  412/1,327] checking    src/Main.els")))
  }

  @Test
  fun readsTheLinesARunOpensAndEndsWithAsProgress() {
    assertEquals(listOf(Progress("eliot · jvm exe-jar · Main", null, null)), parse(listOf("22:24:37 eliot · jvm exe-jar · Main")))
  }

  @Test
  fun ignoresOtherOutput() {
    assertEquals(emptyList<EliotBuildOutputParser.Event>(), parse(listOf("$esc[37m[ INFO] Generated executable jar: /p/Main.jar.")))
  }
}
