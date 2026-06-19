package com.vanillasource.eliot.eliotc.lsp.server

import org.eclipse.lsp4j.{DidChangeConfigurationParams, DidChangeWatchedFilesParams}
import org.eclipse.lsp4j.services.WorkspaceService

/** Workspace-level notifications. `didChangeWatchedFiles` is the on-disk file-watching trigger — the editor watches the
  * workspace and notifies us, which we turn into a recompile. Configuration changes are ignored for now.
  */
final class EliotWorkspaceService(service: EliotCompilationService) extends WorkspaceService {
  override def didChangeConfiguration(params: DidChangeConfigurationParams): Unit = ()

  override def didChangeWatchedFiles(params: DidChangeWatchedFilesParams): Unit = service.requestCompile()
}
