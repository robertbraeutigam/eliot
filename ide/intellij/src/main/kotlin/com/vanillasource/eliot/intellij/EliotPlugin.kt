package com.vanillasource.eliot.intellij

import com.intellij.ide.plugins.PluginManagerCore
import com.intellij.openapi.diagnostic.logger
import com.intellij.openapi.extensions.PluginId
import java.nio.file.Path

/** Shared constants and helpers for locating files this plugin bundles outside its own classpath jar. */
object EliotPlugin {
  /** Must match the `<id>` in plugin.xml. */
  const val PLUGIN_ID = "com.vanillasource.eliot.intellij"

  /** Main class of the bundled Eliot language server (ide/lsp module). */
  const val SERVER_MAIN_CLASS = "com.vanillasource.eliot.eliotc.lsp.server.LspMain"

  private val log = logger<EliotPlugin>()

  /**
   * The plugin's own installation directory. Bundled resources that need a real filesystem path — the
   * server jars (`server/lib/`) and the TextMate grammar (`textmate/`) — are resolved relative to this,
   * since they are shipped as loose files in the distribution, not inside the plugin classpath jar.
   */
  fun pluginPath(): Path? {
    val descriptor = PluginManagerCore.getPlugin(PluginId.getId(PLUGIN_ID))
    if (descriptor == null) log.warn("Eliot plugin descriptor not found for id '$PLUGIN_ID'.")
    return descriptor?.pluginPath
  }
}
