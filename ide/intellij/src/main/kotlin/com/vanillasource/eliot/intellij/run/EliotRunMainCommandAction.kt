package com.vanillasource.eliot.intellij.run

import com.intellij.execution.BeforeRunTaskProvider
import com.intellij.execution.RunManager
import com.intellij.execution.RunManagerEx
import com.intellij.execution.executors.DefaultRunExecutor
import com.intellij.execution.runners.ExecutionUtil
import com.intellij.openapi.actionSystem.ActionUpdateThread
import com.intellij.openapi.actionSystem.AnActionEvent
import com.intellij.openapi.project.Project
import com.redhat.devtools.lsp4ij.commands.LSPCommand
import com.redhat.devtools.lsp4ij.commands.LSPCommandAction

/**
 * Handles the `eliot.runMain` LSP command emitted by the language server's "Run main" code lens.
 *
 * LSP4IJ dispatches a code-lens command client-side by looking up an IntelliJ action whose id equals the
 * command (`ActionManager.getAction("eliot.runMain")`); this action is registered under that id in
 * plugin.xml. The command arguments are `[sourceRoot, moduleName]` — exactly the `<root>` and `-m <module>`
 * the JVM backend needs. From them it creates (or reuses) a native [EliotRunConfiguration], attaches the
 * build-before-run step, and launches it under the Run executor, so the user gets the standard run console,
 * Stop button, and re-run.
 */
class EliotRunMainCommandAction : LSPCommandAction() {
  // Creating/launching a run configuration must happen on the EDT; the base class defaults to a background
  // thread, so request EDT (LSP4IJ then runs commandPerformed on the EDT, directly or via invokeLater).
  override fun getCommandPerformedThread(): ActionUpdateThread = ActionUpdateThread.EDT

  override fun commandPerformed(command: LSPCommand, e: AnActionEvent) {
    val project = e.project ?: return
    val sourceRoot = command.getArgumentAt(0, String::class.java) ?: return
    val mainModule = command.getArgumentAt(1, String::class.java) ?: return
    runMain(project, sourceRoot, mainModule)
  }

  private fun runMain(project: Project, sourceRoot: String, mainModule: String) {
    val runManager = RunManager.getInstance(project)
    val type = EliotRunConfigurationType.getInstance()
    val factory = type.configurationFactories.first()
    val name = "Run $mainModule"

    // Reuse the config for this module across re-runs instead of accumulating duplicates.
    val settings = runManager.findConfigurationByTypeAndName(type, name)
      ?: runManager.createConfiguration(name, factory).also { runManager.addConfiguration(it) }

    val configuration = settings.configuration as EliotRunConfiguration
    configuration.sourceRoot = sourceRoot
    configuration.mainModule = mainModule

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
