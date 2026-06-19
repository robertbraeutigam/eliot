package com.vanillasource.eliot.eliotc.lsp.server

import org.eclipse.lsp4j.{
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
  * roots. There is no build file — the roots from the handshake *are* the project model (stdlib/platform layers come
  * from this process's classpath); see `docs/lsp-server.md`.
  */
final class EliotLanguageServer(service: EliotCompilationService) extends LanguageServer with LanguageClientAware {
  private val textDocumentService        = new EliotTextDocumentService(service)
  private val workspaceService           = new EliotWorkspaceService(service)
  @volatile private var roots: Seq[Path] = Seq.empty

  override def initialize(params: InitializeParams): CompletableFuture[InitializeResult] = {
    roots = workspaceRoots(params)
    val capabilities = new ServerCapabilities()
    capabilities.setTextDocumentSync(TextDocumentSyncKind.Full)
    CompletableFuture.completedFuture(new InitializeResult(capabilities))
  }

  override def initialized(params: InitializedParams): Unit = service.startWorkspace(roots)

  override def shutdown(): CompletableFuture[Object] = {
    service.shutdown()
    CompletableFuture.completedFuture(new Object())
  }

  override def exit(): Unit = ()

  override def getTextDocumentService: TextDocumentService = textDocumentService

  override def getWorkspaceService: WorkspaceService = workspaceService

  override def connect(client: LanguageClient): Unit = service.connect(client)

  // getRootUri is deprecated in LSP in favour of workspaceFolders, but remains the correct fallback for older clients.
  @nowarn("cat=deprecation")
  private def workspaceRoots(params: InitializeParams): Seq[Path] =
    Option(params.getWorkspaceFolders).map(_.asScala.toSeq).filter(_.nonEmpty) match {
      case Some(folders) => folders.map(folder => pathOf(folder.getUri))
      case None          => Option(params.getRootUri).map(pathOf).toSeq
    }

  private def pathOf(uri: String): Path = Paths.get(URI.create(uri))
}
