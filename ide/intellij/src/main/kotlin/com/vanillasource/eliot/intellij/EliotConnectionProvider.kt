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
 *    wildcard), which preserves Eliot's platform-layer resource semantics (a fat jar would collapse
 *    same-path `.els` resources and silently drop a layer).
 *
 * Java itself expands a trailing-star classpath wildcard, so no shell wrapper is needed (works on
 * Windows too). The IDE's bundled JBR is the runtime, so the user needs no separately installed JDK.
 */
class EliotConnectionProvider(project: Project) : OSProcessStreamConnectionProvider() {
  init {
    val pluginPath = EliotPlugin.pluginPath()
      ?: error("Cannot locate the Eliot plugin installation directory; the language server jars are bundled there.")
    val serverLib = pluginPath.resolve("server").resolve("lib")
    // CP1.5: the abstract base + platform layers are no longer found on the classpath; the server scans them as
    // filesystem roots. Point `eliot.layers` at the staged source roots beside server/lib (server/eliot-src) so the
    // server can hand them to the compiler (see EliotCompilationService / BundledLayers).
    val layersDir = serverLib.resolveSibling("eliot-src")

    val command = GeneralCommandLine(
      EliotPlugin.javaExecutable(),
      "-Deliot.layers=$layersDir",
      "-cp",
      serverLib.toString() + File.separator + "*",
      EliotPlugin.SERVER_MAIN_CLASS,
    )
    project.basePath?.let { command.withWorkDirectory(it) }
    super.setCommandLine(command)
  }
}
