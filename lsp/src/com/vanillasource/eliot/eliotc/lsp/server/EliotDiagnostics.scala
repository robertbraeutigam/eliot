package com.vanillasource.eliot.eliotc.lsp.server

import com.vanillasource.eliot.eliotc.feedback.CompilerError
import com.vanillasource.eliot.eliotc.pos.PositionRange
import org.eclipse.lsp4j.{Diagnostic, DiagnosticSeverity, Position, Range}

import java.nio.file.Paths

/** Pure mapping from compiler errors to LSP diagnostics, grouped by document URI.
  *
  * [[CompilerError.contentSource]] is a filesystem *path* (it is built from `URI.getPath`), not a full URI, so we
  * reconstruct a `file:` URI from it to match the document URIs an editor uses. This is reliable for the filesystem
  * source roots the LSP driver walks — it never diagnoses classpath/stdlib resources. Positions are 1-based in the
  * compiler and 0-based in LSP; the compiler's exclusive `to` already matches LSP's exclusive range end.
  */
object EliotDiagnostics {

  /** Group all errors by the document URI they belong to, mapping each to an LSP [[Diagnostic]]. */
  def byUri(errors: Seq[CompilerError]): Map[String, Seq[Diagnostic]] =
    errors.groupMap(uriOf)(toDiagnostic)

  private def uriOf(error: CompilerError): String =
    Paths.get(error.contentSource).toUri.toString

  private def toDiagnostic(error: CompilerError): Diagnostic = {
    val message = if (error.description.isEmpty) error.message else (error.message +: error.description).mkString("\n")
    new Diagnostic(rangeOf(error.sourceRange), message, DiagnosticSeverity.Error, "eliotc")
  }

  private def rangeOf(range: PositionRange): Range =
    new Range(positionOf(range.from.line, range.from.col), positionOf(range.to.line, range.to.col))

  private def positionOf(line: Int, col: Int): Position =
    new Position(math.max(line - 1, 0), math.max(col - 1, 0))
}
