package com.vanillasource.eliot.eliotc.lsp.server

import org.eclipse.lsp4j.{
  DidChangeTextDocumentParams,
  DidCloseTextDocumentParams,
  DidOpenTextDocumentParams,
  DidSaveTextDocumentParams
}
import org.eclipse.lsp4j.services.TextDocumentService

/** Document lifecycle notifications. Opening or saving a document triggers a recompile of the whole workspace (the
  * cancel-restart server coalesces bursts). `didChange` is intentionally a no-op until the virtual file system lands:
  * without it, recompiling on every keystroke would type-check the *on-disk* content, producing diagnostics that
  * disagree with the unsaved buffer — misleading rather than helpful.
  */
final class EliotTextDocumentService(service: EliotCompilationService) extends TextDocumentService {
  override def didOpen(params: DidOpenTextDocumentParams): Unit = service.requestCompile()

  override def didChange(params: DidChangeTextDocumentParams): Unit = () // awaits the VFS overlay; see class doc

  override def didClose(params: DidCloseTextDocumentParams): Unit = ()

  override def didSave(params: DidSaveTextDocumentParams): Unit = service.requestCompile()
}
