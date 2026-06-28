package com.vanillasource.eliot.intellij

import com.intellij.ide.plugins.PluginManagerCore
import com.intellij.openapi.diagnostic.logger
import com.intellij.openapi.extensions.PluginId
import com.intellij.openapi.util.SystemInfo
import java.nio.file.Path

/** Shared constants and helpers for locating files this plugin bundles outside its own classpath jar. */
object EliotPlugin {
  /** Must match the `<id>` in plugin.xml. */
  const val PLUGIN_ID = "com.vanillasource.eliot.intellij"

  /** Main class of the bundled Eliot language server (ide/lsp module). */
  const val SERVER_MAIN_CLASS = "com.vanillasource.eliot.eliotc.lsp.server.LspMain"

  /** Main class of the bundled Eliot compiler CLI (eliotc module), used to build a runnable jar from a `main`. */
  const val COMPILER_MAIN_CLASS = "com.vanillasource.eliot.eliotc.compiler.Main"

  private val log = logger<EliotPlugin>()

  /**
   * The plugin's own installation directory. Bundled resources that need a real filesystem path — the
   * server jars (`server/lib/`), the compiler jars (`compiler/lib/`), and the TextMate grammar (`textmate/`)
   * — are resolved relative to this, since they are shipped as loose files in the distribution, not inside
   * the plugin classpath jar.
   */
  fun pluginPath(): Path? {
    val descriptor = PluginManagerCore.getPlugin(PluginId.getId(PLUGIN_ID))
    if (descriptor == null) log.warn("Eliot plugin descriptor not found for id '$PLUGIN_ID'.")
    return descriptor?.pluginPath
  }

  /** Directory of the per-module language-server jars (`<plugin>/server/lib`). */
  fun serverLibDir(): Path? = pluginPath()?.resolve("server")?.resolve("lib")

  /**
   * Directory of the extra jars the JVM backend needs to build a runnable jar (`<plugin>/compiler/lib`:
   * the jvm module + ASM). The compiler is launched with both this and [serverLibDir] on its classpath.
   */
  fun compilerLibDir(): Path? = pluginPath()?.resolve("compiler")?.resolve("lib")

  /**
   * Directory of the bundled layer `.els` source roots (`<plugin>/server/eliot-src`, staged beside [serverLibDir]),
   * holding a `lang`/`stdlib`/`jvm`/`compiler` subdirectory each. Since CP1.5 the compiler does NOT discover the abstract
   * base or the platform layers on the classpath; the language server and the "Run main" CLI take them as filesystem
   * source roots from here (`eliot.layers` for the server, `--compiler-path`/`--runtime-path` for the CLI — the
   * `compiler` platform layer (CP2) feeds the compiler path only).
   */
  fun bundledLayersDir(): Path? = serverLibDir()?.resolveSibling("eliot-src")

  /** Path to the `java` of the IDE's own runtime (JBR), so no separately installed JDK is required. */
  fun javaExecutable(): String =
    Path.of(System.getProperty("java.home"), "bin", if (SystemInfo.isWindows) "java.exe" else "java").toString()

  /**
   * The compiler/server classpath as a single `-cp` value: a trailing-star wildcard over each of the
   * `server/lib` and `compiler/lib` directories. Java itself expands the wildcards (works on Windows too),
   * and keeping the jars separate preserves Eliot's platform-layer resource semantics (a fat jar would
   * collapse same-path `.els` files). NOTE: avoid writing the literal lib-slash-star here — in a KDoc the
   * slash-star sequence opens a nested block comment and breaks the build.
   */
  fun compilerClasspath(): String? {
    val serverLib = serverLibDir() ?: return null
    val compilerLib = compilerLibDir() ?: return null
    val sep = java.io.File.separator
    return "$serverLib$sep*${java.io.File.pathSeparator}$compilerLib$sep*"
  }
}
