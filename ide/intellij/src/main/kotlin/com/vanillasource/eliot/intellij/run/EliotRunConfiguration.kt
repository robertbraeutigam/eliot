package com.vanillasource.eliot.intellij.run

import com.intellij.execution.ExecutionException
import com.intellij.execution.Executor
import com.intellij.execution.configurations.CommandLineState
import com.intellij.execution.configurations.ConfigurationFactory
import com.intellij.execution.configurations.GeneralCommandLine
import com.intellij.execution.configurations.RunConfiguration
import com.intellij.execution.configurations.RunConfigurationBase
import com.intellij.execution.configurations.RuntimeConfigurationError
import com.intellij.execution.configurations.RunProfileState
import com.intellij.execution.process.OSProcessHandler
import com.intellij.execution.process.ProcessHandler
import com.intellij.execution.process.ProcessTerminatedListener
import com.intellij.execution.runners.ExecutionEnvironment
import com.intellij.openapi.options.SettingsEditor
import com.intellij.openapi.project.Project
import com.vanillasource.eliot.intellij.EliotPlugin
import java.nio.file.Path

/**
 * A native run configuration that builds an Eliot `main` into an executable jar and runs it.
 *
 * The build is a separate [EliotBuildBeforeRunTask] (compiler CLI → `<output>/<module>.jar`); this
 * configuration's own process is the program itself (`java -jar`), so Stop kills the program and the exit
 * code is the program's. The build and run share [resolvedOutputDir] so the run reads exactly what the
 * build wrote.
 */
class EliotRunConfiguration(project: Project, factory: ConfigurationFactory, name: String) :
  RunConfigurationBase<EliotRunConfigurationOptions>(project, factory, name) {

  public override fun getOptions(): EliotRunConfigurationOptions =
    super.getOptions() as EliotRunConfigurationOptions

  var sourceRoot: String?
    get() = options.sourceRoot
    set(value) {
      options.sourceRoot = value
    }

  var mainModule: String?
    get() = options.mainModule
    set(value) {
      options.mainModule = value
    }

  var outputDir: String?
    get() = options.outputDir
    set(value) {
      options.outputDir = value
    }

  override fun getConfigurationEditor(): SettingsEditor<out RunConfiguration> = EliotRunConfigurationEditor()

  override fun checkConfiguration() {
    if (sourceRoot.isNullOrBlank()) throw RuntimeConfigurationError("Source root is not set.")
    if (mainModule.isNullOrBlank()) throw RuntimeConfigurationError("Main module is not set.")
  }

  override fun getState(executor: Executor, environment: ExecutionEnvironment): RunProfileState =
    object : CommandLineState(environment) {
      override fun startProcess(): ProcessHandler {
        val handler = OSProcessHandler(runCommandLine())
        ProcessTerminatedListener.attach(handler)
        return handler
      }
    }

  /** The output directory, defaulting to `<project>/target` (the compiler's own default) when unset. */
  fun resolvedOutputDir(): String =
    outputDir?.takeIf { it.isNotBlank() } ?: Path.of(project.basePath ?: ".", "target").toString()

  /** The executable jar the backend produces for this module: `<output>/<last module segment>.jar`. */
  fun jarPath(): Path = Path.of(resolvedOutputDir(), (mainModule ?: "").substringAfterLast('.') + ".jar")

  /**
   * `eliotc jvm exe-jar <sourceRoot> -m <module> -o <output>`, run as a child JVM off the bundled compiler jars. The
   * abstract base, the standard library and the platform (jvm) layer are NOT bundled with the plugin: like any program's
   * dependencies they must be reachable on the source path — the same roots the resident language server resolved this
   * `main` against. `<sourceRoot>` is that path; a build system will populate it with downloaded packages.
   */
  fun compilerCommandLine(): GeneralCommandLine {
    val classpath = EliotPlugin.compilerClasspath()
      ?: throw ExecutionException("Cannot locate the bundled Eliot compiler jars.")
    val command = GeneralCommandLine(
      EliotPlugin.javaExecutable(),
      "-cp", classpath,
      EliotPlugin.COMPILER_MAIN_CLASS,
      "jvm", "exe-jar",
      sourceRoot.orEmpty(),
      "-m", mainModule.orEmpty(),
      "-o", resolvedOutputDir(),
    )
    project.basePath?.let { command.withWorkDirectory(it) }
    return command
  }

  /** `java -jar <output>/<module>.jar` — the actual program, streamed to the Run console. */
  private fun runCommandLine(): GeneralCommandLine {
    val command = GeneralCommandLine(EliotPlugin.javaExecutable(), "-jar", jarPath().toString())
    project.basePath?.let { command.withWorkDirectory(it) }
    return command
  }
}
