package com.vanillasource.eliot.intellij

import com.intellij.openapi.project.Project
import com.redhat.devtools.lsp4ij.LanguageServerFactory
import com.redhat.devtools.lsp4ij.client.LanguageClientImpl
import com.redhat.devtools.lsp4ij.server.StreamConnectionProvider
import org.eclipse.lsp4j.services.LanguageServer

/**
 * LSP4IJ entry point for the Eliot language server. Registered via the
 * `com.redhat.devtools.lsp4ij.server` extension point in plugin.xml and bound to `*.els` files by the
 * accompanying `fileNamePatternMapping`.
 */
class EliotLanguageServerFactory : LanguageServerFactory {
  override fun createConnectionProvider(project: Project): StreamConnectionProvider =
    EliotConnectionProvider(project)

  override fun createLanguageClient(project: Project): LanguageClientImpl =
    LanguageClientImpl(project)

  override fun getServerInterface(): Class<out LanguageServer> =
    LanguageServer::class.java
}
