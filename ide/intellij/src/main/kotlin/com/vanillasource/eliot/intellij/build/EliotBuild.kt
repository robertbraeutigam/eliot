package com.vanillasource.eliot.intellij.build

import com.intellij.build.BuildDescriptor
import com.intellij.build.BuildViewManager
import com.intellij.build.DefaultBuildDescriptor
import com.intellij.build.FilePosition
import com.intellij.build.events.MessageEvent
import com.intellij.build.progress.BuildProgress
import com.intellij.build.progress.BuildProgressDescriptor
import com.intellij.execution.ExecutionException
import com.intellij.execution.configurations.GeneralCommandLine
import com.intellij.execution.process.OSProcessHandler
import com.intellij.execution.process.ProcessEvent
import com.intellij.execution.process.ProcessListener
import com.intellij.execution.process.ProcessOutputTypes
import com.intellij.openapi.project.Project
import com.intellij.openapi.util.Key
import java.io.File

/**
 * Runs one compile of an Eliot program in the Build tool window, the way a Java build shows there: the compiler's output
 * in the console, each error as an entry that opens its file at the reported position, `--progress` as the running
 * status, and the window brought forward when the build fails.
 *
 * [run] blocks until the compiler exits — it is a before-run step, which the platform runs off the UI thread and gates
 * the launch on.
 */
class EliotBuild(private val project: Project, private val title: String) {
  /** Run [commandLine] (a compiler invocation) and answer whether it succeeded, i.e. exited with 0. */
  fun run(commandLine: GeneralCommandLine): Boolean {
    val progress = BuildViewManager.createBuildProgress(project)
    progress.start(descriptor(Any(), commandLine))
    val handler = try {
      OSProcessHandler(commandLine)
    } catch (e: ExecutionException) {
      return failed(progress, e.message ?: "")
    }
    val parser = EliotBuildOutputParser()
    handler.addProcessListener(object : ProcessListener {
      private val lines = mutableMapOf<Key<*>, StringBuilder>()

      override fun onTextAvailable(event: ProcessEvent, outputType: Key<*>) {
        if (outputType == ProcessOutputTypes.SYSTEM) return
        progress.output(event.text, outputType == ProcessOutputTypes.STDOUT)
        val buffer = lines.getOrPut(outputType) { StringBuilder() }
        buffer.append(event.text)
        while (true) {
          val end = buffer.indexOf("\n")
          if (end < 0) break
          report(progress, parser.accept(buffer.substring(0, end).trimEnd('\r')))
          buffer.delete(0, end + 1)
        }
      }

      override fun processTerminated(event: ProcessEvent) {
        lines.values.filter { it.isNotEmpty() }.forEach { report(progress, parser.accept(it.toString())) }
        report(progress, parser.finish())
      }
    })
    handler.startNotify()
    handler.waitFor()
    val succeeded = handler.exitCode == 0
    if (succeeded) progress.finish() else progress.fail()
    return succeeded
  }

  /** Show a build that could not even start the compiler, e.g. because its jars are missing; answers `false`. */
  fun failToStart(reason: String): Boolean {
    val progress = BuildViewManager.createBuildProgress(project)
    progress.start(descriptor(Any(), null))
    return failed(progress, reason)
  }

  private fun failed(progress: BuildProgress<BuildProgressDescriptor>, reason: String): Boolean {
    progress.message("Cannot start the Eliot compiler", reason, MessageEvent.Kind.ERROR, null)
    progress.fail()
    return false
  }

  private fun report(progress: BuildProgress<BuildProgressDescriptor>, events: List<EliotBuildOutputParser.Event>) {
    for (event in events) {
      when (event) {
        is EliotBuildOutputParser.Problem -> {
          val file = File(event.file)
          if (file.isFile) {
            progress.fileMessage(event.message, event.detail, MessageEvent.Kind.ERROR, FilePosition(file, event.line - 1, event.column - 1))
          } else {
            progress.message(event.message, event.detail, MessageEvent.Kind.ERROR, null)
          }
        }
        is EliotBuildOutputParser.Progress ->
          if (event.done != null && event.total != null) {
            progress.progress(event.text, event.total, event.done, "facts")
          } else {
            progress.progress(event.text)
          }
      }
    }
  }

  private fun descriptor(id: Any, commandLine: GeneralCommandLine?): BuildProgressDescriptor {
    val build = DefaultBuildDescriptor(id, title, commandLine?.workDirectory?.path ?: project.basePath.orEmpty(), System.currentTimeMillis())
    build.isActivateToolWindowWhenFailed = true
    return object : BuildProgressDescriptor {
      override fun getTitle(): String = title

      override fun getBuildDescriptor(): BuildDescriptor = build
    }
  }
}
