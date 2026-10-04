package com.vanillasource.eliot.intellij.run

import com.intellij.psi.PsiDocumentManager
import com.intellij.psi.PsiElement
import com.intellij.psi.PsiFile
import com.redhat.devtools.lsp4ij.LSPFileSupport
import com.redhat.devtools.lsp4ij.LSPIJUtils
import com.redhat.devtools.lsp4ij.features.codeLens.CodeLensDataResult
import org.eclipse.lsp4j.CodeLensParams
import org.eclipse.lsp4j.TextDocumentIdentifier
import java.util.Collections
import java.util.WeakHashMap
import java.util.concurrent.CompletableFuture

/**
 * The [EliotRunTarget]s of a file, read from the language server's code lenses through LSP4IJ.
 *
 * Both readers — the gutter and the run-configuration producer — run inside a read action, where waiting on the server
 * is not allowed. So this never waits: LSP4IJ keeps one lens request per file until the document changes (the same one
 * its own code-vision provider uses), and while it is still in flight this answers "nothing yet" and has the file's
 * highlighting restarted once the answer arrives, which asks the gutter again.
 */
object EliotRunTargets {
  /** The requests a highlighting restart is already attached to, so each one is attached once, not once per element. */
  private val awaited: MutableSet<CompletableFuture<*>> = Collections.synchronizedSet(Collections.newSetFromMap(WeakHashMap()))

  /** The run targets of [file], or an empty list when it is no `.els` file or the server has not answered yet. */
  fun of(file: PsiFile): List<EliotRunTarget> {
    if (file.virtualFile?.extension != "els") return emptyList()
    val support = LSPFileSupport.getSupport(file)
    val request = support.codeLensSupport.getCodeLenses(CodeLensParams(TextDocumentIdentifier()))
    if (!request.isDone) {
      if (awaited.add(request)) request.whenComplete { _, _ -> support.restartDaemonCodeAnalyzerWithDebounce() }
      return emptyList()
    }
    if (request.isCompletedExceptionally || request.isCancelled) return emptyList()
    return targetsOf(request.join())
  }

  /** The target whose lens starts inside [element], which the gutter icon is anchored to. */
  fun startingIn(element: PsiElement): EliotRunTarget? {
    val file = element.containingFile ?: return null
    val document = PsiDocumentManager.getInstance(file.project).getDocument(file) ?: return null
    val range = element.textRange
    return of(file).firstOrNull { range.contains(LSPIJUtils.toOffset(it.range.start, document)) }
  }

  /**
   * The target the editor context at [element] means: the one on its line, else the first in the file — so "Run" from
   * anywhere in a module with a `main` runs that `main`, as it does for a Java class.
   */
  fun around(element: PsiElement): EliotRunTarget? {
    val file = element.containingFile ?: return null
    val document = PsiDocumentManager.getInstance(file.project).getDocument(file) ?: return null
    val targets = of(file)
    val line = document.getLineNumber(element.textOffset.coerceIn(0, document.textLength))
    return targets.firstOrNull { it.range.start.line == line } ?: targets.minByOrNull { it.range.start.line }
  }

  private fun targetsOf(result: CodeLensDataResult): List<EliotRunTarget> =
    result.codeLensData.mapNotNull { EliotRunTarget.of(it.codeLens) }
}
