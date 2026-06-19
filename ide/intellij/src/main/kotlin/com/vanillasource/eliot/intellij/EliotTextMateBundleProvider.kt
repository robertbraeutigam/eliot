package com.vanillasource.eliot.intellij

import com.intellij.openapi.diagnostic.logger
import org.jetbrains.plugins.textmate.api.TextMateBundleProvider
import org.jetbrains.plugins.textmate.api.TextMateBundleProvider.PluginBundle
import kotlin.io.path.isDirectory

/**
 * Registers the bundled Eliot TextMate grammar (a VS Code-extension-layout bundle shipped at
 * `<plugin>/textmate/`, a build-time copy of ide/textmate) so `.els` files are highlighted with no
 * manual TextMate-bundle setup. Contributed through the `com.intellij.textmate.bundleProvider` EP.
 */
class EliotTextMateBundleProvider : TextMateBundleProvider {
  private val log = logger<EliotTextMateBundleProvider>()

  override fun getBundles(): List<PluginBundle> {
    val bundle = EliotPlugin.pluginPath()?.resolve("textmate")
    if (bundle == null || !bundle.isDirectory()) {
      log.warn("Eliot TextMate bundle not found at expected location: $bundle")
      return emptyList()
    }
    return listOf(PluginBundle("Eliot", bundle))
  }
}
