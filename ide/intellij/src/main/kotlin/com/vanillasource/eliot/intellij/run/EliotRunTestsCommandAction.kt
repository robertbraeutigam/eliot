package com.vanillasource.eliot.intellij.run

import com.intellij.openapi.actionSystem.ActionUpdateThread
import com.intellij.openapi.actionSystem.AnActionEvent
import com.redhat.devtools.lsp4ij.commands.LSPCommand
import com.redhat.devtools.lsp4ij.commands.LSPCommandAction

/**
 * Handles the `eliot.runTests` LSP command emitted by the language server's "Run tests" code lens, which sits above a
 * module's `testCases`.
 *
 * The command arguments are `[sourceRoot, moduleName, dependencyRoot*]`, exactly those of `eliot.runMain`, where the
 * module is the one holding the suite. The program that runs is not that module but the test runner
 * ([EliotRunLauncher.TEST_RUNNER_MODULE]), built over the same roots; the suite's module name is its one program
 * argument, which makes the runner run that suite alone (`eliot.test.Arguments`: a name selects the suites whose module
 * is that name or lies below it). `--format=teamcity` makes it print TeamCity service messages, which the platform's
 * test runner shows as a results tree (module → subject → case, with a diff for a failed comparison); see
 * [EliotRunConfiguration] and [EliotTestConsoleProperties]. The runner exits non-zero when a case fails, so the run
 * ends as a failed one.
 *
 * Registered in plugin.xml under the id `eliot.runTests`, which must equal the command: LSP4IJ dispatches a code-lens
 * command client-side via `ActionManager.getAction(commandId)`.
 */
class EliotRunTestsCommandAction : LSPCommandAction() {
  // See EliotRunMainCommandAction: launching a run configuration has to happen on the EDT.
  override fun getCommandPerformedThread(): ActionUpdateThread = ActionUpdateThread.EDT

  override fun commandPerformed(command: LSPCommand, e: AnActionEvent) {
    val project = e.project ?: return
    val call = EliotRunLauncher.callOf(command) ?: return
    EliotRunLauncher.launch(
      project,
      name = "Test ${call.moduleName}",
      sourceRoot = call.sourceRoot,
      mainModule = EliotRunLauncher.TEST_RUNNER_MODULE,
      programArguments = "--format=teamcity ${call.moduleName}",
      dependencyRoots = call.dependencyRoots,
      testRun = true,
    )
  }
}
