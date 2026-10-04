package com.vanillasource.eliot.intellij.run

import com.intellij.execution.BeforeRunTaskProvider
import com.intellij.execution.actions.ConfigurationContext
import com.intellij.execution.actions.LazyRunConfigurationProducer
import com.intellij.execution.configurations.ConfigurationFactory
import com.intellij.openapi.util.Ref
import com.intellij.psi.PsiElement

/**
 * Makes an [EliotRunConfiguration] from where the user is in the editor — the gutter icon, the editor's context menu,
 * Ctrl+Shift+F10 — so an Eliot `main` or suite runs the way a Java one does: a temporary configuration the first time,
 * reused (not duplicated) afterwards, and kept for good once saved.
 *
 * What is run is the [EliotRunTarget] at the context's element ([EliotRunTargets.around]): the one on its line, else
 * the file's first. Every configuration made here gets the build step ([EliotBuildBeforeRunTask]), so a stale jar is
 * never run.
 */
class EliotRunConfigurationProducer : LazyRunConfigurationProducer<EliotRunConfiguration>() {
  override fun getConfigurationFactory(): ConfigurationFactory =
    EliotRunConfigurationType.getInstance().configurationFactories.first()

  override fun setupConfigurationFromContext(
    configuration: EliotRunConfiguration,
    context: ConfigurationContext,
    sourceElement: Ref<PsiElement>,
  ): Boolean {
    val element = context.psiLocation ?: return false
    val target = EliotRunTargets.around(element) ?: return false
    target.applyTo(configuration)
    attachBuildTask(configuration)
    return true
  }

  override fun isConfigurationFromContext(configuration: EliotRunConfiguration, context: ConfigurationContext): Boolean {
    val element = context.psiLocation ?: return false
    return EliotRunTargets.around(element)?.isRunBy(configuration) ?: false
  }

  /** Make the build step the configuration's one before-run task. */
  private fun attachBuildTask(configuration: EliotRunConfiguration) {
    val provider = BeforeRunTaskProvider.getProvider(configuration.project, EliotBuildBeforeRunTaskProvider.ID) ?: return
    val task = provider.createTask(configuration) ?: return
    configuration.beforeRunTasks = listOf(task)
  }
}
