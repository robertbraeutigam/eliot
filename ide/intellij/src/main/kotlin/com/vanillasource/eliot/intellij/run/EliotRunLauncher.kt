package com.vanillasource.eliot.intellij.run

import com.intellij.execution.BeforeRunTaskProvider
import com.intellij.execution.RunManager
import com.intellij.execution.RunManagerEx
import com.intellij.execution.executors.DefaultRunExecutor
import com.intellij.execution.runners.ExecutionUtil
import com.intellij.openapi.project.Project
import com.redhat.devtools.lsp4ij.commands.LSPCommand
import java.io.File

/**
 * What the language server's run lenses have in common: the arguments they carry, and turning them into a launched
 * [EliotRunConfiguration].
 *
 * Both lenses (`eliot.runMain`, `eliot.runTests`) carry `[buildRoot, moduleName, dependencyRoot*]`. A main runs the
 * module's own `main`; a test run builds the test runner ([TEST_RUNNER_MODULE]) over the same roots and starts it with
 * the suite's module name as its argument, which selects exactly that suite.
 */
object EliotRunLauncher {
  /** The module whose `main` runs the suites of a program — the `suite` package's `compiler run -m` line. */
  const val TEST_RUNNER_MODULE = "eliot.test.Runner"

  /** A lens command's arguments: the root holding the file, the module, and every other root of the package. */
  data class LensCall(val sourceRoot: String, val moduleName: String, val dependencyRoots: List<String>)

  /**
   * The call a lens command carries, or null when it lacks the root or the module.
   *
   * The remaining arguments are the dependency source roots (layer/library roots), variable in number. The loop is
   * bounded by the actual argument count: LSP4IJ's `getArgumentAt` throws (not returns null) once the index reaches the
   * list size, so probing past the end for a null terminator would abort the command.
   */
  fun callOf(command: LSPCommand): LensCall? {
    val sourceRoot = command.getArgumentAt(0, String::class.java) ?: return null
    val moduleName = command.getArgumentAt(1, String::class.java) ?: return null
    val dependencyRoots = (2 until command.arguments.size)
      .mapNotNull { command.getArgumentAt(it, String::class.java) }
    return LensCall(sourceRoot, moduleName, dependencyRoots)
  }

  /**
   * Create (or reuse) the configuration called [name], point it at the given build, attach the build-before-run step and
   * launch it under the Run executor, so the user gets the standard run console, Stop button, and re-run. A
   * configuration is reused across re-runs instead of accumulating duplicates, so everything that distinguishes one run
   * from the last is set here, [programArguments] and [testRun] included. A [testRun] shows the runner's output as a results
   * tree instead of console text, so [programArguments] must make the program print what that tree reads
   * (`--format=teamcity`).
   */
  fun launch(
    project: Project,
    name: String,
    sourceRoot: String,
    mainModule: String,
    programArguments: String,
    dependencyRoots: List<String>,
    testRun: Boolean = false,
  ) {
    val runManager = RunManager.getInstance(project)
    val type = EliotRunConfigurationType.getInstance()
    val factory = type.configurationFactories.first()

    val settings = runManager.findConfigurationByTypeAndName(type, name)
      ?: runManager.createConfiguration(name, factory).also { runManager.addConfiguration(it) }

    val configuration = settings.configuration as EliotRunConfiguration
    configuration.sourceRoot = sourceRoot
    configuration.mainModule = mainModule
    configuration.programArguments = programArguments
    configuration.testRun = testRun
    configuration.dependencyPath = dependencyRoots.joinToString(File.pathSeparator)

    attachBuildTask(project, configuration)
    runManager.selectedConfiguration = settings
    ExecutionUtil.runConfiguration(settings, DefaultRunExecutor.getRunExecutorInstance())
  }

  /** Ensure the build-the-jar step runs before launch (idempotent: one such task on the configuration). */
  private fun attachBuildTask(project: Project, configuration: EliotRunConfiguration) {
    val provider = BeforeRunTaskProvider.getProvider(project, EliotBuildBeforeRunTaskProvider.ID) ?: return
    val task = provider.createTask(configuration) ?: return
    task.isEnabled = true
    (RunManager.getInstance(project) as RunManagerEx).setBeforeRunTasks(configuration, listOf(task))
  }
}
