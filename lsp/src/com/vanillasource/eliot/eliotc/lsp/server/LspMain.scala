package com.vanillasource.eliot.eliotc.lsp.server

import cats.effect.unsafe.IORuntime
import org.eclipse.lsp4j.launch.LSPLauncher

/** Standalone entry point for the Eliot language server: speaks LSP (JSON-RPC) over this process's stdin/stdout, which
  * is how an editor — the client — drives a server it spawned. All logging is routed to stderr (see
  * `resources/log4j2.xml`) so it cannot corrupt the stdout protocol stream.
  */
object LspMain {
  def main(args: Array[String]): Unit = {
    val service  = new EliotCompilationService(IORuntime.global)
    val server   = new EliotLanguageServer(service)
    val launcher = LSPLauncher.createServerLauncher(server, System.in, System.out)
    server.connect(launcher.getRemoteProxy)
    val _        = launcher.startListening().get() // blocks until the client closes the input stream
  }
}
