package com.vanillasource.eliot.intellij.run

import com.intellij.openapi.progress.ProgressManager
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
import java.util.concurrent.CancellationException
import java.util.concurrent.CompletableFuture
import java.util.concurrent.ExecutionException
import java.util.concurrent.TimeUnit
import java.util.concurrent.TimeoutException

/**
 * The [EliotRunTarget]s of a file, read from the language server's code lenses through LSP4IJ, which keeps one lens
 * request per file until the document changes (the same one its own code-vision provider uses). A file that is not open
 * is connected to the server by the request itself, and the server answers from the indices it already has.
 *
 * The two readers treat a request still in flight differently, because only one of them is asked again:
 * - the gutter ([of]) never waits — it answers "nothing yet" and has the file's highlighting restarted once the answer
 *   arrives, which asks it again;
 * - the run-configuration producer ([around] with a wait) is asked once per context menu or keystroke, so it waits a
 *   bounded [PRODUCER_WAIT_MILLIS] — cancellably, since it runs in a read action — or a file not yet open, whose request
 *   only starts with that question, would never offer a run.
 */
object EliotRunTargets {
  /** The requests a highlighting restart is already attached to, so each one is attached once, not once per element. */
  private val awaited: MutableSet<CompletableFuture<*>> = Collections.synchronizedSet(Collections.newSetFromMap(WeakHashMap()))

  /** How long the producer waits for a lens request still in flight before offering nothing. */
  private const val PRODUCER_WAIT_MILLIS = 2_000L

  /**
   * The run targets of [file], or an empty list when it is no `.els` file or the server has not answered within
   * [waitMillis] (none by default).
   */
  fun of(file: PsiFile, waitMillis: Long = 0): List<EliotRunTarget> {
    if (file.virtualFile?.extension != "els") return emptyList()
    val support = LSPFileSupport.getSupport(file)
    val request = support.codeLensSupport.getCodeLenses(CodeLensParams(TextDocumentIdentifier()))
    awaitCancellably(request, waitMillis)
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
   * The target the context at [element] means: the one on its line, else the first in the file — so "Run" from anywhere
   * in a module with a `main`, or on the file in the project view, runs that `main`, as it does for a Java class. Waits
   * for the server as the producer does (see the class comment).
   */
  fun around(element: PsiElement): EliotRunTarget? {
    val file = element.containingFile ?: return null
    val document = PsiDocumentManager.getInstance(file.project).getDocument(file) ?: return null
    val targets = of(file, PRODUCER_WAIT_MILLIS)
    val line = document.getLineNumber(element.textOffset.coerceIn(0, document.textLength))
    return targets.firstOrNull { it.range.start.line == line } ?: targets.minByOrNull { it.range.start.line }
  }

  /** Wait up to [millis] for [request], giving up early when the read action this runs in is cancelled. */
  private fun awaitCancellably(request: CompletableFuture<*>, millis: Long) {
    val deadline = System.nanoTime() + TimeUnit.MILLISECONDS.toNanos(millis)
    while (!request.isDone && System.nanoTime() < deadline) {
      ProgressManager.checkCanceled()
      try {
        request.get(POLL_MILLIS, TimeUnit.MILLISECONDS)
      } catch (_: TimeoutException) {
      } catch (_: ExecutionException) {
      } catch (_: CancellationException) {
      }
    }
  }

  private const val POLL_MILLIS = 20L

  private fun targetsOf(result: CodeLensDataResult): List<EliotRunTarget> =
    result.codeLensData.mapNotNull { EliotRunTarget.of(it.codeLens) }
}
