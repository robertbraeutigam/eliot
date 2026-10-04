package com.vanillasource.eliot.intellij.build

import com.intellij.build.BuildViewManager
import com.intellij.build.events.BuildEvent
import com.intellij.build.events.FileMessageEvent
import com.intellij.build.events.FinishBuildEvent
import com.intellij.build.events.impl.FailureResultImpl
import com.intellij.build.events.impl.SuccessResultImpl
import com.intellij.execution.configurations.GeneralCommandLine
import com.intellij.testFramework.replaceService
import com.intellij.testFramework.fixtures.BasePlatformTestCase
import java.io.File
import java.nio.file.Files
import java.nio.file.Path
import java.util.Collections

/**
 * Runs the real compiler — the one `ide/lsp/package.sh` builds for the plugin — and reads what reaches the Build tool
 * window.
 */
class EliotBuildTest : BasePlatformTestCase() {
  private val repository: Path = Path.of("../..").toAbsolutePath().normalize()
  private val events: MutableList<BuildEvent> = Collections.synchronizedList(mutableListOf())

  /** Headless, the platform's view manager discards every event; this one keeps them for the test to read. */
  override fun setUp() {
    super.setUp()
    val recorder = object : BuildViewManager(project) {
      override fun onEvent(buildId: Any, event: BuildEvent) {
        events.add(event)
      }
    }
    project.replaceService(BuildViewManager::class.java, recorder, testRootDisposable)
  }

  private fun compile(sourceRoot: Path, module: String): Boolean =
    EliotBuild(project, "Build $module").run(
      GeneralCommandLine(
        "java", "-cp", listOf("lib", "compiler-lib").joinToString(File.pathSeparator) { "$repository/ide/lsp/dist/$it/*" },
        "com.vanillasource.eliot.eliotc.compiler.Main", "jvm", "exe-jar", sourceRoot.toString(), "-m", module,
        "-o", Files.createTempDirectory("eliot-build-out").toString(), "--progress",
        "--path", "$repository/lang/eliot/src", "--path", "$repository/stdlib/eliot/src", "--path", "$repository/jvm/eliot/src",
      ),
    )

  private fun brokenProgram(): Path {
    val root = Files.createTempDirectory("eliot-build-src")
    Files.writeString(root.resolve("Broken.els"), "def main: {Console} Unit = printLine(undefinedName)\n")
    return root
  }

  fun testAFailedBuildReportsEachErrorOnceAtItsPositionAndFails() {
    val root = brokenProgram()
    assertFalse(compile(root, "Broken"))
    assertEquals(
      listOf("${root.resolve("Broken.els")}:0:37 Name not defined."),
      events.filterIsInstance<FileMessageEvent>().map { "${it.filePosition.file}:${it.filePosition.startLine}:${it.filePosition.startColumn} ${it.message}" },
    )
  }

  fun testAFailedBuildFinishesAsAFailure() {
    compile(brokenProgram(), "Broken")
    assertTrue(events.filterIsInstance<FinishBuildEvent>().single().result is FailureResultImpl)
  }

  fun testASuccessfulBuildFinishesAsASuccess() {
    assertTrue(compile(repository.resolve("examples/src"), "HelloWorld"))
    assertTrue(events.filterIsInstance<FinishBuildEvent>().single().result is SuccessResultImpl)
  }
}
