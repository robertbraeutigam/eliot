package com.vanillasource.eliot.intellij

import com.intellij.execution.configurations.GeneralCommandLine
import com.intellij.openapi.project.Project
import com.redhat.devtools.lsp4ij.server.OSProcessStreamConnectionProvider
import java.io.File

/**
 * Launches the bundled Eliot language server as a child JVM and speaks LSP over its stdin/stdout.
 *
 * The server is run out-of-process on purpose:
 *  - it isolates the server's own lsp4j / gson / log4j / cats-effect from the IDE's (LSP4IJ ships its
 *    own lsp4j, so running in-process would clash); and
 *  - it lets us point the classpath at the directory of *separate* per-module jars (a trailing-star
 *    wildcard), which keeps each layer's same-path `META-INF/services/...CompilerPlugin` file distinct
 *    (a fat jar would collapse them and silently drop plugin registrations).
 *
 * Java itself expands a trailing-star classpath wildcard, so no shell wrapper is needed (works on
 * Windows too). The IDE's bundled JBR is the runtime, so the user needs no separately installed JDK.
 */
class EliotConnectionProvider(project: Project) : OSProcessStreamConnectionProvider() {
  init {
    val pluginPath = EliotPlugin.pluginPath()
      ?: error("Cannot locate the Eliot plugin installation directory; the language server jars are bundled there.")
    val serverLib = pluginPath.resolve("server").resolve("lib")
    // The server bundles no layer sources: the abstract base, the standard library and the platform layers reach the
    // compiler on the path as dependencies. The client sends its workspace roots on `initialize`, and that path is
    // where those layers live (see EliotCompilationService). So the launch is code-only — just the classpath.
    val command = GeneralCommandLine(
      EliotPlugin.javaExecutable(),
      "-cp",
      serverLib.toString() + File.separator + "*",
      EliotPlugin.SERVER_MAIN_CLASS,
    )
    project.basePath?.let { command.withWorkDirectory(it) }
    super.setCommandLine(command)
  }
}
