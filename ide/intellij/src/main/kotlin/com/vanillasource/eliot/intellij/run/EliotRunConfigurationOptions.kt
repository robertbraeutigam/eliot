package com.vanillasource.eliot.intellij.run

import com.intellij.execution.configurations.RunConfigurationOptions

/**
 * Persisted settings of an [EliotRunConfiguration]: the source root passed to the compiler, the module
 * that declares `main` (the backend's `-m` argument), and the output directory the executable jar is
 * written to and run from. Stored via the platform's options mechanism (round-tripped to the run
 * configuration XML automatically).
 */
class EliotRunConfigurationOptions : RunConfigurationOptions() {
  private val sourceRootOption = string("").provideDelegate(this, "sourceRoot")
  private val mainModuleOption = string("").provideDelegate(this, "mainModule")
  private val outputDirOption = string("").provideDelegate(this, "outputDir")

  var sourceRoot: String?
    get() = sourceRootOption.getValue(this)
    set(value) = sourceRootOption.setValue(this, value)

  var mainModule: String?
    get() = mainModuleOption.getValue(this)
    set(value) = mainModuleOption.setValue(this, value)

  var outputDir: String?
    get() = outputDirOption.getValue(this)
    set(value) = outputDirOption.setValue(this, value)
}
