package com.vanillasource.eliot.eliotc.lsp.server

import org.eclipse.lsp4j.{
  CodeLensOptions,
  CompletionOptions,
  InitializeParams,
  InitializeResult,
  InitializedParams,
  ServerCapabilities,
  TextDocumentSyncKind
}
import org.eclipse.lsp4j.services.{
  LanguageClient,
  LanguageClientAware,
  LanguageServer,
  TextDocumentService,
  WorkspaceService
}

import java.net.URI
import java.nio.file.{Path, Paths}
import java.util.concurrent.CompletableFuture
import scala.annotation.nowarn
import scala.jdk.CollectionConverters.*

/** The lsp4j entry object: it advertises capabilities, captures the workspace roots from the `initialize` handshake
  * (`workspaceFolders`, falling back to `rootUri`), and on `initialized` starts the resident compile engine over those
  * roots. There is no build file — the roots from the handshake *are* the project model, and the standard library and
  * platform layers are just more roots on that same path (dependencies the client already has on disk), never bundled
  * with the server.
  */
final class EliotLanguageServer(service: EliotCompilationService) extends LanguageServer with LanguageClientAware {
  private val textDocumentService           = new EliotTextDocumentService(service)
  private val workspaceService              = new EliotWorkspaceService(service)
  @volatile private var roots: Seq[Path]    = Seq.empty
  @volatile private var canRegisterWatchers = false
  @volatile private var canRefreshCodeLenses = false

  override def initialize(params: InitializeParams): CompletableFuture[InitializeResult] = {
    roots = workspaceRoots(params)
    canRegisterWatchers = supportsWatchedFileRegistration(params)
    canRefreshCodeLenses = supportsCodeLensRefresh(params)
    val capabilities = new ServerCapabilities()
    capabilities.setTextDocumentSync(TextDocumentSyncKind.Full)
    capabilities.setDefinitionProvider(true)
    capabilities.setHoverProvider(true)
    // No trigger characters: the editor invokes completion on identifier input / explicit request, and the whole
    // in-scope list is returned at once (`isIncomplete = false`) for the client to filter by the typed prefix.
    capabilities.setCompletionProvider(new CompletionOptions(false, java.util.Collections.emptyList()))
    // Code lenses surface a "Run main" affordance above each runnable `main`; the lenses are returned fully resolved
    // (command attached), so no separate resolve step is advertised.
    capabilities.setCodeLensProvider(new CodeLensOptions(false))
    CompletableFuture.completedFuture(new InitializeResult(capabilities))
  }

  override def initialized(params: InitializedParams): Unit = {
    service.startWorkspace(roots)
    if (canRegisterWatchers) service.registerFileWatchers()
    if (canRefreshCodeLenses) service.enableCodeLensRefresh()
  }

  override def shutdown(): CompletableFuture[Object] = {
    service.shutdown()
    CompletableFuture.completedFuture(new Object())
  }

  override def exit(): Unit = ()

  override def getTextDocumentService: TextDocumentService = textDocumentService

  override def getWorkspaceService: WorkspaceService = workspaceService

  override def connect(client: LanguageClient): Unit = service.connect(client)

  // `workspace/didChangeWatchedFiles` is registration-only in LSP, and registration requires the client to support
  // dynamic registration for it. Default to off when the capability tree is absent (older/minimal clients).
  private def supportsWatchedFileRegistration(params: InitializeParams): Boolean =
    Option(params.getCapabilities)
      .flatMap(caps => Option(caps.getWorkspace))
      .flatMap(ws => Option(ws.getDidChangeWatchedFiles))
      .exists(dcwf => java.lang.Boolean.TRUE == dcwf.getDynamicRegistration)

  // `workspace/codeLens/refresh` lets the server ask the client to re-pull code lenses after an async recompile; per LSP
  // it must only be sent when the client advertised `workspace.codeLens.refreshSupport`. Off when the tree is absent.
  private def supportsCodeLensRefresh(params: InitializeParams): Boolean =
    Option(params.getCapabilities)
      .flatMap(caps => Option(caps.getWorkspace))
      .flatMap(ws => Option(ws.getCodeLens))
      .exists(cl => java.lang.Boolean.TRUE == cl.getRefreshSupport)

  // getRootUri is deprecated in LSP in favour of workspaceFolders, but remains the correct fallback for older clients.
  @nowarn("cat=deprecation")
  private def workspaceRoots(params: InitializeParams): Seq[Path] =
    Option(params.getWorkspaceFolders).map(_.asScala.toSeq).filter(_.nonEmpty) match {
      case Some(folders) => folders.map(folder => pathOf(folder.getUri))
      case None          => Option(params.getRootUri).map(pathOf).toSeq
    }

  private def pathOf(uri: String): Path = Paths.get(URI.create(uri))
}
