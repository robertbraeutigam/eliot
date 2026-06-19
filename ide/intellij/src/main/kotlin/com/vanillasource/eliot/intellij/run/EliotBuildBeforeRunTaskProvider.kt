package com.vanillasource.eliot.intellij.run

import com.intellij.execution.BeforeRunTaskProvider
import com.intellij.execution.ExecutionException
import com.intellij.execution.process.CapturingProcessHandler
import com.intellij.execution.runners.ExecutionEnvironment
import com.intellij.execution.configurations.RunConfiguration
import com.intellij.icons.AllIcons
import com.intellij.notification.NotificationGroupManager
import com.intellij.notification.NotificationType
import com.intellij.openapi.actionSystem.DataContext
import com.intellij.openapi.project.Project
import com.intellij.openapi.util.Key
import javax.swing.Icon

/**
 * Runs the Eliot compiler to build an [EliotRunConfiguration]'s executable jar before the program is
 * launched. The compile runs as a child JVM off the bundled jars (`server/lib` + `compiler/lib`); a
 * non-zero exit aborts the launch (so a stale jar is never run) and the compiler's diagnostics are shown
 * as an error notification. Registered via the `com.intellij.stepsBeforeRunProvider` extension point.
 */
class EliotBuildBeforeRunTaskProvider : BeforeRunTaskProvider<EliotBuildBeforeRunTask>() {
  override fun getId(): Key<EliotBuildBeforeRunTask> = ID

  override fun getName(): String = "Build Eliot executable jar"

  override fun getIcon(): Icon = AllIcons.Actions.Compile

  override fun createTask(runConfiguration: RunConfiguration): EliotBuildBeforeRunTask? =
    if (runConfiguration is EliotRunConfiguration) EliotBuildBeforeRunTask() else null

  override fun executeTask(
    context: DataContext,
    configuration: RunConfiguration,
    environment: ExecutionEnvironment,
    task: EliotBuildBeforeRunTask,
  ): Boolean {
    if (configuration !is EliotRunConfiguration) return true
    return try {
      val output = CapturingProcessHandler(configuration.compilerCommandLine()).runProcess(BUILD_TIMEOUT_MS)
      if (output.exitCode == 0) {
        true
      } else {
        notifyFailure(configuration.project, (output.stdout + "\n" + output.stderr).trim())
        false
      }
    } catch (e: ExecutionException) {
      notifyFailure(configuration.project, e.message ?: "Failed to start the Eliot compiler.")
      false
    }
  }

  private fun notifyFailure(project: Project, details: String) {
    NotificationGroupManager.getInstance()
      .getNotificationGroup("Eliot")
      .createNotification("Eliot build failed", details.ifBlank { "See build output." }, NotificationType.ERROR)
      .notify(project)
  }

  companion object {
    val ID: Key<EliotBuildBeforeRunTask> = Key.create("Eliot.BuildExecutableJar")
    private const val BUILD_TIMEOUT_MS = 120_000
  }
}
