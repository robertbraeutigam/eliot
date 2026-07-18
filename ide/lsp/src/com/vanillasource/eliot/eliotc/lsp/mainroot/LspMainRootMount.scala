package com.vanillasource.eliot.eliotc.lsp.mainroot

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.processor.CompilerIO.CompilerIO
import com.vanillasource.eliot.eliotc.source.scan.SourceMount

import java.net.URI
import java.nio.file.Path

/** Mounts the LSP's synthesized per-module monomorphization roots (`lspmain.M` wrapping workspace module `M`) into the
  * runtime scan pool, resolved to the `lsp-main:` namespace served by [[LspMainRootSourceProcessor]]. Contributed by
  * the LSP plugin via [[com.vanillasource.eliot.eliotc.source.scan.PathScanner.extraRuntimeMountsKey]].
  *
  * `enumerate` deliberately lists nothing: the wrapper modules exist only on demand (the type-hint driver names them
  * explicitly), and they must stay invisible to whole-pool enumeration — `namedValues` reflection must never gather
  * from synthesized wrappers.
  */
final class LspMainRootMount extends SourceMount {

  override def resolve(path: Path): CompilerIO[Option[URI]] =
    Option
      .when(LspMainRootSourceProcessor.isWrapperPath(path))(LspMainRootSourceProcessor.uriFor(path))
      .pure[CompilerIO]

  override def enumerate: CompilerIO[Seq[Path]] = Seq.empty[Path].pure[CompilerIO]

  override def toString: String = s"LspMainRootMount(${LspMainRootSourceProcessor.scheme}:)"
}
