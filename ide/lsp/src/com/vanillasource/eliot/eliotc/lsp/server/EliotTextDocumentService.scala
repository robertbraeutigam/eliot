package com.vanillasource.eliot.eliotc.lsp.server

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.resolve.fact.ResolvedValue
import org.eclipse.lsp4j.jsonrpc.messages.Either as JEither
import org.eclipse.lsp4j.services.TextDocumentService
import org.eclipse.lsp4j.{
  DefinitionParams,
  DidChangeTextDocumentParams,
  DidCloseTextDocumentParams,
  DidOpenTextDocumentParams,
  DidSaveTextDocumentParams,
  Hover,
  HoverParams,
  Location,
  LocationLink,
  MarkupContent,
  MarkupKind
}

import java.net.URI
import java.util
import java.util.concurrent.CompletableFuture
import scala.jdk.CollectionConverters.*

/** Document lifecycle notifications and position-based requests.
  *
  * Opening or saving a document triggers a recompile of the whole workspace (the cancel-restart server coalesces
  * bursts). `didChange` is intentionally a no-op until the virtual file system lands: without it, recompiling on every
  * keystroke would type-check the *on-disk* content, producing diagnostics that disagree with the unsaved buffer —
  * misleading rather than helpful.
  *
  * Definition and hover are answered synchronously from the [[com.vanillasource.eliot.eliotc.lsp.index.PositionIndex]]
  * of the latest finished compile. The editor's 0-based positions are converted to the compiler's 1-based domain via
  * [[LspPositions]] before querying, and the resulting compiler ranges converted back.
  */
final class EliotTextDocumentService(service: EliotCompilationService) extends TextDocumentService {
  override def didOpen(params: DidOpenTextDocumentParams): Unit = service.requestCompile()

  override def didChange(params: DidChangeTextDocumentParams): Unit = () // awaits the VFS overlay; see class doc

  override def didClose(params: DidCloseTextDocumentParams): Unit = ()

  override def didSave(params: DidSaveTextDocumentParams): Unit = service.requestCompile()

  override def definition(
      params: DefinitionParams
  ): CompletableFuture[JEither[util.List[? <: Location], util.List[? <: LocationLink]]] = {
    val uri      = URI.create(params.getTextDocument.getUri)
    val position = LspPositions.toCompilerPosition(params.getPosition)
    val targets  = service.positionIndex
      .definitionAt(uri, position)
      .map(target => new Location(target.uri.toString, LspPositions.toRange(target.range)))
      .toList
    CompletableFuture.completedFuture(
      JEither.forLeft[util.List[? <: Location], util.List[? <: LocationLink]](targets.asJava)
    )
  }

  override def hover(params: HoverParams): CompletableFuture[Hover] = {
    val uri      = URI.create(params.getTextDocument.getUri)
    val position = LspPositions.toCompilerPosition(params.getPosition)
    val hover    = service.positionIndex.hoverAt(uri, position).map { (reference, value) =>
      val contents = new MarkupContent(MarkupKind.PLAINTEXT, EliotTextDocumentService.signatureOf(value))
      new Hover(contents, LspPositions.toRange(reference.range))
    }
    CompletableFuture.completedFuture(hover.orNull)
  }
}

object EliotTextDocumentService {

  /** A one-line "name : type" rendering of a resolved value, for hover. Uses the bottom level of the type stack — the
    * type of the runtime value — which is the signature a reader expects to see.
    */
  private def signatureOf(value: ResolvedValue): String =
    s"${value.name.value.show} : ${value.typeStack.value.signature.show}"
}
