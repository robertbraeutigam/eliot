package com.vanillasource.eliot.intellij.run

import com.google.gson.JsonPrimitive
import org.eclipse.lsp4j.CodeLens
import org.eclipse.lsp4j.Range
import java.io.File

/**
 * Something in an `.els` file that can be run: a module's `main`, or a suite (`testCases`) run by the test runner.
 *
 * The language server decides what is runnable and reports it as a code lens — `eliot.runMain` or `eliot.runTests` with
 * the arguments `[buildRoot, moduleName, dependencyRoot*]` — which is what every other editor shows as text above the
 * line. IntelliJ reads the same lenses and shows them its own way: a gutter icon ([EliotRunLineMarkerContributor]) and
 * a run configuration made from the editor context ([EliotRunConfigurationProducer]). So "is this a test" stays the
 * runner's own rule, decided once, in the server.
 *
 * A main runs the module's own `main`; a suite builds the test runner ([TEST_RUNNER_MODULE]) over the same roots and
 * starts it with `--format=teamcity <module>`, which selects exactly that suite and prints the service messages the
 * platform's test console reads into a results tree (see [EliotRunConfiguration], [EliotTestConsoleProperties]).
 */
data class EliotRunTarget(
  val kind: Kind,
  val range: Range,
  val sourceRoot: String,
  val moduleName: String,
  val dependencyRoots: List<String>,
) {
  enum class Kind(val command: String) {
    MAIN("eliot.runMain"),
    TESTS("eliot.runTests"),
  }

  /** The name a configuration made from this target gets: the module for a main, "Tests in …" for a suite. */
  val configurationName: String
    get() = when (kind) {
      Kind.MAIN -> moduleName
      Kind.TESTS -> "Tests in $moduleName"
    }

  private val mainModule: String
    get() = when (kind) {
      Kind.MAIN -> moduleName
      Kind.TESTS -> TEST_RUNNER_MODULE
    }

  private val programArguments: String
    get() = when (kind) {
      Kind.MAIN -> ""
      Kind.TESTS -> "--format=teamcity $moduleName"
    }

  private val dependencyPath: String
    get() = dependencyRoots.joinToString(File.pathSeparator)

  /** Point [configuration] at this target, overwriting everything that distinguishes one target's run from another's. */
  fun applyTo(configuration: EliotRunConfiguration) {
    configuration.name = configurationName
    configuration.sourceRoot = sourceRoot
    configuration.mainModule = mainModule
    configuration.programArguments = programArguments
    configuration.testRun = kind == Kind.TESTS
    configuration.dependencyPath = dependencyPath
  }

  /** Whether [configuration] runs exactly this target, so the platform reuses it instead of creating a duplicate. */
  fun isRunBy(configuration: EliotRunConfiguration): Boolean =
    configuration.sourceRoot == sourceRoot &&
      configuration.mainModule == mainModule &&
      configuration.programArguments.orEmpty() == programArguments &&
      configuration.testRun == (kind == Kind.TESTS) &&
      configuration.dependencyPath.orEmpty() == dependencyPath

  companion object {
    /** The module whose `main` runs the suites of a program — the `suite` package's `compiler run -m` line. */
    const val TEST_RUNNER_MODULE = "eliot.test.Runner"

    /**
     * The target a lens reports, or null when the lens is not one of Eliot's run lenses or lacks the root or module.
     *
     * The arguments arrive as lsp4j deserialized them: Gson elements, not strings, since a lens command's arguments are
     * untyped `Object`s.
     */
    fun of(lens: CodeLens): EliotRunTarget? {
      val command = lens.command ?: return null
      val kind = Kind.entries.firstOrNull { it.command == command.command } ?: return null
      val arguments = command.arguments.orEmpty().map(::stringOf)
      val sourceRoot = arguments.getOrNull(0) ?: return null
      val moduleName = arguments.getOrNull(1) ?: return null
      return EliotRunTarget(kind, lens.range, sourceRoot, moduleName, arguments.drop(2).filterNotNull())
    }

    private fun stringOf(argument: Any?): String? = when (argument) {
      is String -> argument
      is JsonPrimitive -> if (argument.isString) argument.asString else null
      else -> null
    }
  }
}
