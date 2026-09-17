package com.vanillasource.eliot.eliotc.lsp.server

import org.eclipse.lsp4j.{DidChangeConfigurationParams, DidChangeWatchedFilesParams}
import org.eclipse.lsp4j.services.WorkspaceService

import java.net.URI
import scala.jdk.CollectionConverters.*

/** Workspace-level notifications. `didChangeWatchedFiles` is the on-disk file-watching trigger — the editor watches the
  * workspace and notifies us. A change to a file the workspace plan was read from (a project's `eliot.pkg` or
  * `eliot.lock`) replans every session; any other change recompiles the sessions the changed files concern.
  * Configuration changes are ignored for now.
  */
final class EliotWorkspaceService(service: EliotCompilationService) extends WorkspaceService {
  override def didChangeConfiguration(params: DidChangeConfigurationParams): Unit = ()

  override def didChangeWatchedFiles(params: DidChangeWatchedFilesParams): Unit = {
    val changed = params.getChanges.asScala.toSeq.map(change => URI.create(change.getUri))
    if (changed.exists(service.isPlanInput)) service.reloadWorkspace() else service.requestCompile(changed)
  }
}
