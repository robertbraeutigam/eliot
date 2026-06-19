package com.vanillasource.eliot.intellij.run

import com.intellij.execution.configurations.ConfigurationFactory
import com.intellij.execution.configurations.ConfigurationType
import com.intellij.execution.configurations.RunConfiguration
import com.intellij.openapi.project.Project

/** Creates [EliotRunConfiguration] instances and declares their persisted [EliotRunConfigurationOptions]. */
class EliotRunConfigurationFactory(type: ConfigurationType) : ConfigurationFactory(type) {
  override fun getId(): String = "Eliot Application"

  override fun createTemplateConfiguration(project: Project): RunConfiguration =
    EliotRunConfiguration(project, this, "Eliot")

  override fun getOptionsClass(): Class<EliotRunConfigurationOptions> = EliotRunConfigurationOptions::class.java
}
