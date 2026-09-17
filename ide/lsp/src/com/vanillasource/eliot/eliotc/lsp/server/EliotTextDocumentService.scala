package com.vanillasource.eliot.eliotc.lsp.server

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.lsp.index.CompletionIndex
import com.vanillasource.eliot.eliotc.monomorphize.fact.GroundValueRenderer
import com.vanillasource.eliot.eliotc.pos.{Position, PositionRange}
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
    val uri      = URI.create(document.getUri)
    service.virtualFileSystem.update(uri, document.getText)
    service.requestCompile(uri)
  }

  override def didChange(params: DidChangeTextDocumentParams): Unit =
    params.getContentChanges.asScala.lastOption.foreach { change =>
      val uri = URI.create(params.getTextDocument.getUri)
      service.virtualFileSystem.update(uri, change.getText)
      service.requestCompile(uri)
    }

  override def didClose(params: DidCloseTextDocumentParams): Unit = {
    val uri = URI.create(params.getTextDocument.getUri)
    service.virtualFileSystem.remove(uri)
    service.requestCompile(uri)
  }

  override def didSave(params: DidSaveTextDocumentParams): Unit =
    service.requestCompile(URI.create(params.getTextDocument.getUri))

  override def definition(
      params: DefinitionParams
  ): CompletableFuture[JEither[util.List[? <: Location], util.List[? <: LocationLink]]] = {
    val uri      = URI.create(params.getTextDocument.getUri)
    val position = LspPositions.toCompilerPosition(params.getPosition)
    val targets  = service
      .indicesFor(uri)
      .view
      .flatMap(_.position.definitionAt(uri, position))
      .headOption
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
    val items = service
      .indicesFor(uri)
      .view
      .map(_.completion.completionsAt(uri))
      .find(_.nonEmpty)
      .getOrElse(Seq.empty)
      .map(EliotTextDocumentService.toCompletionItem)
      .asJava
    // `isIncomplete = false`: the full in-scope list is returned once and the client filters it as the user types.
    CompletableFuture.completedFuture(
      JEither.forRight[util.List[CompletionItem], CompletionList](new CompletionList(false, items))
    )
  }

  /** A hover renders, as Markdown, the apidoc tile of the identifier under the cursor: its *definition* in a fenced
    * `eliot` block, then the *concrete type it was checked at here*, then its documentation.
    *
    * When the referenced name has a documentation tile ([[com.vanillasource.eliot.eliotc.lsp.index.DocIndex]], built
    * from the same apidoc facts the site uses), the header is its rendered definition (`def printLine(s: String):
    * IO[Unit]`, `type IO[A]`, `ability Show[A]`) — exactly what the doc page shows — and, when the expression was
    * monomorphized, the concrete type at this use site (the type-hint index — `String -> IO[Unit]`, an instantiation
    * `Int[0, 255] -> Int[0, 255]`, one line per instantiation) is shown below it, then the documentation. With no tile
    * it falls back to the concrete type alone, or the referenced value's declared signature when the node was never
    * monomorphized.
    *
    * The packages with the file on their path are asked in [[EliotCompilationService.indicesFor]]'s order, except that one
    * holding a concrete type at the cursor goes first: code is monomorphized only in a package that runs a `main`
    * reaching it, which is often not the package the file belongs to.
    */
  override def hover(params: HoverParams): CompletableFuture[Hover] = {
    val uri      = URI.create(params.getTextDocument.getUri)
    val position = LspPositions.toCompilerPosition(params.getPosition)
    val indices  = service.indicesFor(uri)
    val hinted   = indices.find(_.typeHint.typeHintsAt(uri, position).isDefined)
    val ordered  = hinted.toSeq ++ indices.filterNot(candidate => hinted.exists(_ eq candidate))
    val hover    = ordered.view.flatMap(EliotTextDocumentService.hoverFrom(_, uri, position)).headOption
    CompletableFuture.completedFuture(hover.orNull)
  }

  /** Offer a "Run main" lens above each runnable `main`. A lens is emitted only when the document declares a `main`
    * ([[com.vanillasource.eliot.eliotc.lsp.index.MainIndex]]) *and* some package has the file on its path — the root
    * holding the file, paired with the module name, is exactly what the JVM backend needs to build the program
    * (`exe-jar <root> -m <module>`). Which package's path is used is [[EliotCompilationService.runTargetFor]]'s choice:
    * the one whose closure can run the `main`. The command arguments are `[buildRoot, moduleName, dependencyRoot*]`: the
    * build root, the module, then every *other* root of that package — the layer/library roots the build must put on the
    * path, since none is bundled. The client (the IntelliJ plugin) launches a native run configuration from them; the
    * command is handled client-side (no `executeCommandProvider` is advertised).
    */
  override def codeLens(params: CodeLensParams): CompletableFuture[util.List[? <: CodeLens]] = {
    val uri    = URI.create(params.getTextDocument.getUri)
    val lenses = service.runTargetFor(uri).toList.map { target =>
      val arguments = (List[Object](target.root.toString, target.entry.moduleName.show) ++
        target.dependencyRoots.map(_.toString: Object)).asJava
      new CodeLens(LspPositions.toRange(target.entry.range), new Command("▶ Run main", "eliot.runMain", arguments), null)
    }
    CompletableFuture.completedFuture(lenses.asJava)
  }
}

object EliotTextDocumentService {

  /** The hover `indices` give for `position` in `uri`, if any — see [[EliotTextDocumentService.hover]] for what it
    * shows.
    */
  private def hoverFrom(indices: PackageSession.Indices, uri: URI, position: Position): Option[Hover] = {
    val reference = indices.position.referenceAt(uri, position)
    val tile      = reference.flatMap(occurrence => indices.doc.tileFor(occurrence.value).map(occurrence -> _))
    val typeHint  = indices.typeHint.typeHintsAt(uri, position)
    val interval  = typeHint.flatMap { case (range, _) => indices.typeHint.intervalAt(uri, range) }
    tile match {
      case Some((occurrence, entry)) =>
        val definition    = entry.signature
          .orElse(indices.position.definitionOf(occurrence.value).map(signatureOf))
          .getOrElse(occurrence.value.name.name)
        val concreteTypes = typeHint.fold(Seq.empty[String])(_._2.map(GroundValueRenderer.render))
        Some(renderHover(occurrence.range, Some(definition), concreteTypes, interval, entry.doc))
      case None                      =>
        typeHint match {
          case Some((range, types)) =>
            Some(renderHover(range, None, types.map(GroundValueRenderer.render), interval, None))
          case None                 =>
            indices.position.hoverAt(uri, position).map { (occurrence, value) =>
              renderHover(occurrence.range, Some(signatureOf(value)), Seq.empty, None, None)
            }
        }
    }
  }

  /** Render a hover as Markdown from up to four parts, omitting any that is absent (never an empty block).
    *
    * With a `definition`, it leads the fenced `eliot` block; the `concreteTypes` (the type(s) the expression was
    * checked at here) then follow as a labelled inline line, then — when the refinement channel pinned one — the
    * value's range `[min, max]` at this node, and the `doc` below. Without a definition — the fallback path — the
    * concrete type(s) become the primary fenced block instead. Rendered as [[MarkupKind.MARKDOWN]] so signatures are
    * syntax-highlighted and the doc's own formatting shows, matching the apidoc site.
    */
  private def renderHover(
      range: PositionRange,
      definition: Option[String],
      concreteTypes: Seq[String],
      interval: Option[(BigInt, BigInt)],
      doc: Option[String]
  ): Hover = {
    val rangeLine = interval.map { case (min, max) => s"_value range:_ `[$min, $max]`" }
    val sections  = definition match {
      case Some(value) =>
        val typeLine =
          Option.when(concreteTypes.nonEmpty)(s"_at this use:_ ${concreteTypes.map(tpe => s"`$tpe`").mkString(", ")}")
        Seq(Some(s"```eliot\n$value\n```"), typeLine, rangeLine, doc.filter(_.nonEmpty))
      case None        =>
        Seq(
          Option.when(concreteTypes.nonEmpty)(s"```eliot\n${concreteTypes.mkString("\n")}\n```"),
          rangeLine,
          doc.filter(_.nonEmpty)
        )
    }
    new Hover(new MarkupContent(MarkupKind.MARKDOWN, sections.flatten.mkString("\n\n")), LspPositions.toRange(range))
  }

  /** A one-line "name : type" rendering of a resolved value, for hover. Uses the value's signature — the type of the
    * runtime value — which is what a reader expects to see.
    */
  private def signatureOf(value: ResolvedValue): String =
    s"${value.name.value.show} : ${value.signature.value.render}"

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
