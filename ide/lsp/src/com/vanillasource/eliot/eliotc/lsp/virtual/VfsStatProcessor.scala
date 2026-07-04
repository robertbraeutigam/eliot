package com.vanillasource.eliot.eliotc.lsp.virtual

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.processor.CompilerIO.CompilerIO
import com.vanillasource.eliot.eliotc.processor.common.SingleFactProcessor

/** Produces the total [[VfsStat]] leaf for any file from the in-memory [[VirtualFileSystem]] overlay. */
class VfsStatProcessor(vfs: VirtualFileSystem) extends SingleFactProcessor[VfsStat.Key] {
  override protected def generateSingleFact(key: VfsStat.Key): CompilerIO[VfsStat] =
    VfsStat(key.file, vfs.get(key.file).map(_.stamp)).pure[CompilerIO]
}
