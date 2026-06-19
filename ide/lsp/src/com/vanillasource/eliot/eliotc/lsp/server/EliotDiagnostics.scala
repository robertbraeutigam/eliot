package com.vanillasource.eliot.eliotc.lsp.server

import com.vanillasource.eliot.eliotc.feedback.CompilerError
import org.eclipse.lsp4j.{Diagnostic, DiagnosticSeverity}

import java.nio.file.Paths

/** Pure mapping from compiler errors to LSP diagnostics, grouped by document URI.
  *
  * [[CompilerError.contentSource]] is a filesystem *path* (it is built from `URI.getPath`), not a full URI, so we
  * reconstruct a `file:` URI from it to match the document URIs an editor uses. This is reliable for the filesystem
  * source roots the LSP driver walks — it never diagnoses classpath/stdlib resources. Position conversion (1-based
  * compiler → 0-based LSP) is shared with the position-based features via [[LspPositions]].
  */
object EliotDiagnostics {

  /** Group all errors by the document URI they belong to, mapping each to an LSP [[Diagnostic]]. */
  def byUri(errors: Seq[CompilerError]): Map[String, Seq[Diagnostic]] =
    errors.groupMap(uriOf)(toDiagnostic)

  private def uriOf(error: CompilerError): String =
    Paths.get(error.contentSource).toUri.toString

  private def toDiagnostic(error: CompilerError): Diagnostic = {
    val message = if (error.description.isEmpty) error.message else (error.message +: error.description).mkString("\n")
    new Diagnostic(LspPositions.toRange(error.sourceRange), message, DiagnosticSeverity.Error, "eliotc")
  }
}
