package com.vanillasource.eliot.eliotc.lsp.server

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.lsp.index.{CompletionIndex, GroundValueRenderer}
import com.vanillasource.eliot.eliotc.pos.PositionRange
import com.vanillasource.eliot.eliotc.resolve.fact.ResolvedValue
import org.eclipse.lsp4j.jsonrpc.messages.Either as JEither
import org.eclipse.lsp4j.services.TextDocumentService
import org.eclipse.lsp4j.{
  CodeLens,
  CodeLensParams,
  Command,
  CompletionItem,
  CompletionItemKind,
  CompletionList,
  CompletionParams,
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
  * Edits are kept live through the virtual file system: `didOpen` and `didChange` push the buffer's full text into the
  * overlay (the server negotiates [[TextDocumentSyncKind.Full]], so each change carries the whole document), and
  * `didClose` drops the override so the compiler reverts to the on-disk file. Every notification then triggers a
  * recompile of the whole workspace (the cancel-restart server coalesces keystroke bursts); because the overlay already
  * holds the latest buffer, the recompile type-checks what the user sees, not stale disk content. `didSave` only needs
  * to recompile — the saved text equals the override already recorded by the preceding `didChange`.
  *
  * Definition and hover are answered synchronously from the [[com.vanillasource.eliot.eliotc.lsp.index.PositionIndex]]
  * of the latest finished compile. The editor's 0-based positions are converted to the compiler's 1-based domain via
  * [[LspPositions]] before querying, and the resulting compiler ranges converted back.
  */
final class EliotTextDocumentService(service: EliotCompilationService) extends TextDocumentService {
  override def didOpen(params: DidOpenTextDocumentParams): Unit = {
    val document = params.getTextDocument
    service.virtualFileSystem.update(URI.create(document.getUri), document.getText)
    service.requestCompile()
  }

  override def didChange(params: DidChangeTextDocumentParams): Unit =
    params.getContentChanges.asScala.lastOption.foreach { change =>
      service.virtualFileSystem.update(URI.create(params.getTextDocument.getUri), change.getText)
      service.requestCompile()
    }

  override def didClose(params: DidCloseTextDocumentParams): Unit = {
    service.virtualFileSystem.remove(URI.create(params.getTextDocument.getUri))
    service.requestCompile()
  }

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

  override def completion(
      params: CompletionParams
  ): CompletableFuture[JEither[util.List[CompletionItem], CompletionList]] = {
    val uri   = URI.create(params.getTextDocument.getUri)
    val items = service.completionIndex.completionsAt(uri).map(EliotTextDocumentService.toCompletionItem).asJava
    // `isIncomplete = false`: the full in-scope list is returned once and the client filters it as the user types.
    CompletableFuture.completedFuture(
      JEither.forRight[util.List[CompletionItem], CompletionList](new CompletionList(false, items))
    )
  }

  /** A hover shows, as Markdown, a code header plus the identifier's API documentation.
    *
    * The header prefers the concrete monomorphic type at the hovered node (the type-hint index), which shows the type
    * the expression was actually checked at — `Int[0, 255]`, `IO[Unit]` — and falls back to the referenced value's
    * declared signature (the position index) when the node was never monomorphized (no reachable `main`, or code the
    * `main` does not reach). The documentation is the referenced name's [[com.vanillasource.eliot.eliotc.lsp.index.DocIndex]]
    * entry — the same margin-stripped Markdown the apidoc site renders — appended below the header when present.
    */
  override def hover(params: HoverParams): CompletableFuture[Hover] = {
    val uri       = URI.create(params.getTextDocument.getUri)
    val position  = LspPositions.toCompilerPosition(params.getPosition)
    val reference = service.positionIndex.referenceAt(uri, position)
    val doc       = reference.flatMap(occurrence => service.docIndex.docFor(occurrence.value))
    val hover     = service.typeHintIndex.typeHintsAt(uri, position) match {
      case Some((range, types)) =>
        Some(EliotTextDocumentService.markdownHover(range, types.map(GroundValueRenderer.render).mkString("\n"), doc))
      case None                 =>
        service.positionIndex.hoverAt(uri, position).map { (occurrence, value) =>
          EliotTextDocumentService.markdownHover(occurrence.range, EliotTextDocumentService.signatureOf(value), doc)
        }
    }
    CompletableFuture.completedFuture(hover.orNull)
  }

  /** Offer a "Run main" lens above each runnable `main`. A lens is emitted only when the document declares a `main`
    * ([[com.vanillasource.eliot.eliotc.lsp.index.MainIndex]]) *and* the file sits under a known workspace source root —
    * the root paired with the module name is exactly what the JVM backend needs to build the program (`exe-jar <root>
    * -m <module>`). Both are carried as command arguments so the client (the IntelliJ plugin) can launch a native run
    * configuration; the command itself is handled client-side (no `executeCommandProvider` is advertised).
    */
  override def codeLens(params: CodeLensParams): CompletableFuture[util.List[? <: CodeLens]] = {
    val uri    = URI.create(params.getTextDocument.getUri)
    val lenses = for {
      entry <- service.mainIndex.mainAt(uri).toList
      root  <- service.sourceRootFor(uri).toList
    } yield {
      val arguments = List[Object](root.toString, entry.moduleName.show).asJava
      new CodeLens(LspPositions.toRange(entry.range), new Command("▶ Run main", "eliot.runMain", arguments), null)
    }
    CompletableFuture.completedFuture(lenses.asJava)
  }
}

object EliotTextDocumentService {

  /** A Markdown hover: the code header in a fenced `eliot` block, followed (when the name is documented) by its
    * documentation Markdown. Rendered as [[MarkupKind.MARKDOWN]] so the signature is syntax-highlighted and the doc's
    * own formatting (paragraphs, code, lists) shows, matching the apidoc site.
    */
  private def markdownHover(range: PositionRange, header: String, doc: Option[String]): Hover = {
    val fenced   = s"```eliot\n$header\n```"
    val markdown = doc.filter(_.nonEmpty).fold(fenced)(documentation => s"$fenced\n\n$documentation")
    new Hover(new MarkupContent(MarkupKind.MARKDOWN, markdown), LspPositions.toRange(range))
  }

  /** A one-line "name : type" rendering of a resolved value, for hover. Uses the bottom level of the type stack — the
    * type of the runtime value — which is the signature a reader expects to see.
    */
  private def signatureOf(value: ResolvedValue): String =
    s"${value.name.value.show} : ${value.typeStack.value.signature.show}"

  /** Map an in-scope-name entry to an lsp4j completion item: the bare name is both label and insert text, the kind
    * drives the icon, and the rendered signature (when known) becomes the detail shown alongside.
    */
  private def toCompletionItem(entry: CompletionIndex.Entry): CompletionItem = {
    val item = new CompletionItem(entry.name)
    item.setKind(completionKind(entry.kind))
    entry.detail.foreach(item.setDetail)
    item
  }

  private def completionKind(kind: CompletionIndex.Kind): CompletionItemKind = kind match {
    case CompletionIndex.Kind.Value         => CompletionItemKind.Function
    case CompletionIndex.Kind.Type          => CompletionItemKind.Class
    case CompletionIndex.Kind.AbilityMethod => CompletionItemKind.Method
  }
}
