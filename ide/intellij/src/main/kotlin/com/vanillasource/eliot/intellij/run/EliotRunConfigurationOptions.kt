package com.vanillasource.eliot.intellij.run

import com.intellij.execution.configurations.RunConfigurationOptions

/**
 * Persisted settings of an [EliotRunConfiguration]: the source root passed to the compiler, the module
 * that declares `main` (the backend's `-m` argument), the dependency roots put on the compiler path (the
 * layer/library roots, path-separator-joined — none is bundled), and the output directory the executable
 * jar is written to and run from, and the arguments the program is started with (a test run's module name).
 * Stored via the platform's options mechanism (round-tripped to the run
 * configuration XML automatically).
 */
class EliotRunConfigurationOptions : RunConfigurationOptions() {
  private val sourceRootOption = string("").provideDelegate(this, "sourceRoot")
  private val mainModuleOption = string("").provideDelegate(this, "mainModule")
  private val dependencyPathOption = string("").provideDelegate(this, "dependencyPath")
  private val outputDirOption = string("").provideDelegate(this, "outputDir")
  private val programArgumentsOption = string("").provideDelegate(this, "programArguments")

  var sourceRoot: String?
    get() = sourceRootOption.getValue(this)
    set(value) = sourceRootOption.setValue(this, value)

  var mainModule: String?
    get() = mainModuleOption.getValue(this)
    set(value) = mainModuleOption.setValue(this, value)

  /** The dependency source roots to put on the compiler path, joined by the platform path separator. */
  var dependencyPath: String?
    get() = dependencyPathOption.getValue(this)
    set(value) = dependencyPathOption.setValue(this, value)

  var outputDir: String?
    get() = outputDirOption.getValue(this)
    set(value) = outputDirOption.setValue(this, value)

  /** The command line the program is started with after `java -jar <jar>`, parsed with the platform's shell-like quoting. */
  var programArguments: String?
    get() = programArgumentsOption.getValue(this)
    set(value) = programArgumentsOption.setValue(this, value)
}
