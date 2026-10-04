package com.vanillasource.eliot.intellij.run

import com.google.gson.JsonPrimitive
import org.eclipse.lsp4j.CodeLens
import org.eclipse.lsp4j.Command
import org.eclipse.lsp4j.Position
import org.eclipse.lsp4j.Range
import org.junit.Assert.assertEquals
import org.junit.Assert.assertNull
import org.junit.Test

class EliotRunTargetTest {
  private val range = Range(Position(3, 4), Position(3, 8))

  private fun lens(command: String, vararg arguments: Any) = CodeLens(range, Command("▶", command, arguments.toList()), null)

  @Test
  fun readsAMainLensWithGsonArguments() {
    assertEquals(
      EliotRunTarget(EliotRunTarget.Kind.MAIN, range, "/p/src", "my.Main", listOf("/c/stdlib", "/c/jvm")),
      EliotRunTarget.of(lens("eliot.runMain", JsonPrimitive("/p/src"), JsonPrimitive("my.Main"), JsonPrimitive("/c/stdlib"), JsonPrimitive("/c/jvm"))),
    )
  }

  @Test
  fun readsATestsLensWithStringArguments() {
    assertEquals(EliotRunTarget(EliotRunTarget.Kind.TESTS, range, "/p/test", "my.Tests", emptyList()), EliotRunTarget.of(lens("eliot.runTests", "/p/test", "my.Tests")))
  }

  @Test
  fun ignoresAnotherCommand() {
    assertNull(EliotRunTarget.of(lens("other.command", "/p/src", "my.Main")))
  }

  @Test
  fun ignoresALensWithoutAModule() {
    assertNull(EliotRunTarget.of(lens("eliot.runMain", "/p/src")))
  }
}
