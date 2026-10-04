package com.vanillasource.eliot.intellij

import com.intellij.codeInsight.codeVision.CodeVisionEntry
import com.redhat.devtools.lsp4ij.client.features.LSPCodeLensFeature
import com.vanillasource.eliot.intellij.run.EliotRunTarget
import org.eclipse.lsp4j.CodeLens

/**
 * Hides the server's run lenses (`▶ Run main`, `▶ Run tests`), which IntelliJ shows natively instead: a gutter icon
 * and editor run actions (see [EliotRunTarget]). The server still sends them — that is what other editors show — and
 * the plugin still reads them; only LSP4IJ's code-vision text is withheld. Any other lens is shown as usual.
 */
class EliotCodeLensFeature : LSPCodeLensFeature() {
  override fun createCodeVisionEntry(codeLens: CodeLens, providerId: String, context: LSPCodeLensContext): CodeVisionEntry? =
    if (EliotRunTarget.of(codeLens) != null) null else super.createCodeVisionEntry(codeLens, providerId, context)
}
