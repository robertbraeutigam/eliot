package com.vanillasource.eliot.intellij.run

import com.intellij.execution.configurations.ConfigurationTypeBase
import com.intellij.execution.configurations.ConfigurationTypeUtil
import com.intellij.icons.AllIcons
import com.intellij.openapi.util.NotNullLazyValue

/**
 * The "Eliot Application" run configuration type — what shows up under Run/Debug Configurations and in the
 * Run menu. Registered via the `com.intellij.configurationType` extension point in plugin.xml.
 */
class EliotRunConfigurationType : ConfigurationTypeBase(
  ID,
  "Eliot Application",
  "Builds and runs an Eliot main as an executable jar",
  NotNullLazyValue.createValue { AllIcons.Actions.Execute },
) {
  init {
    addFactory(EliotRunConfigurationFactory(this))
  }

  companion object {
    const val ID = "EliotApplicationRunConfiguration"

    fun getInstance(): EliotRunConfigurationType =
      ConfigurationTypeUtil.findConfigurationType(EliotRunConfigurationType::class.java)
  }
}
