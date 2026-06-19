package com.vanillasource.eliot.intellij.run

import com.intellij.openapi.options.SettingsEditor
import com.intellij.ui.components.JBTextField
import com.intellij.util.ui.FormBuilder
import javax.swing.JComponent
import javax.swing.JPanel

/**
 * The editor form for an [EliotRunConfiguration]: source root, main module, and (optional) output
 * directory. Leaving the output directory blank uses the compiler default, `<project>/target`.
 */
class EliotRunConfigurationEditor : SettingsEditor<EliotRunConfiguration>() {
  private val sourceRootField = JBTextField()
  private val mainModuleField = JBTextField()
  private val outputDirField = JBTextField()

  override fun resetEditorFrom(configuration: EliotRunConfiguration) {
    sourceRootField.text = configuration.sourceRoot.orEmpty()
    mainModuleField.text = configuration.mainModule.orEmpty()
    outputDirField.text = configuration.outputDir.orEmpty()
  }

  override fun applyEditorTo(configuration: EliotRunConfiguration) {
    configuration.sourceRoot = sourceRootField.text
    configuration.mainModule = mainModuleField.text
    configuration.outputDir = outputDirField.text
  }

  override fun createEditor(): JComponent =
    FormBuilder.createFormBuilder()
      .addLabeledComponent("Source root:", sourceRootField)
      .addLabeledComponent("Main module:", mainModuleField)
      .addLabeledComponent("Output directory (blank = <project>/target):", outputDirField)
      .addComponentFillVertically(JPanel(), 0)
      .panel
}
