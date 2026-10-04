package com.vanillasource.eliot.intellij.run

import com.intellij.openapi.actionSystem.ActionUpdateThread
import com.intellij.openapi.actionSystem.AnActionEvent
import com.redhat.devtools.lsp4ij.commands.LSPCommand
import com.redhat.devtools.lsp4ij.commands.LSPCommandAction

/**
 * Handles the `eliot.runMain` LSP command emitted by the language server's "Run main" code lens.
 *
 * LSP4IJ dispatches a code-lens command client-side by looking up an IntelliJ action whose id equals the
 * command (`ActionManager.getAction("eliot.runMain")`); this action is registered under that id in
 * plugin.xml. The command arguments are `[sourceRoot, moduleName, dependencyRoot*]` — the `<root>` and
 * `-m <module>` the JVM backend needs, then every other discovered source root (the layer/library roots to
 * put on the compiler path, since none is bundled). From them [EliotRunLauncher] creates (or reuses) a native
 * [EliotRunConfiguration], attaches the build-before-run step, and launches it under the Run executor.
 */
class EliotRunMainCommandAction : LSPCommandAction() {
  // Creating/launching a run configuration must happen on the EDT; the base class defaults to a background
  // thread, so request EDT (LSP4IJ then runs commandPerformed on the EDT, directly or via invokeLater).
  override fun getCommandPerformedThread(): ActionUpdateThread = ActionUpdateThread.EDT

  override fun commandPerformed(command: LSPCommand, e: AnActionEvent) {
    val project = e.project ?: return
    val call = EliotRunLauncher.callOf(command) ?: return
    EliotRunLauncher.launch(
      project,
      name = "Run ${call.moduleName}",
      sourceRoot = call.sourceRoot,
      mainModule = call.moduleName,
      programArguments = "",
      dependencyRoots = call.dependencyRoots,
    )
  }
}
