package com.vanillasource.eliot.eliotc.apidoc.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.apidoc.build.DocText
import com.vanillasource.eliot.eliotc.apidoc.fact.ValueDoc
import com.vanillasource.eliot.eliotc.ast.fact.SourceAST
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.SingleFactProcessor
import com.vanillasource.eliot.eliotc.source.stat.FileStat

import java.nio.file.Path

/** Produces the merged documentation ([[ValueDoc]]) for a fully-qualified name, purely from the parsed ASTs — the same
  * front-end source (the `/** ... */` comments the parser attaches to declarations) the apidoc site is built from, so
  * the compiler proper (which drops docs at the core boundary) is never involved.
  *
  * It resolves the name's module to a file under every layer root — the compiler/runtime/user roots, each paired with a
  * layer label, captured at construction exactly as `PathScanner` captures its roots — reads each layer's [[SourceAST]],
  * takes the declaration's doc comment ([[DocText.docOf]]), and merges them with [[DocText.selectDoc]] (lowest layer
  * wins, duplicates warned). A layer with no such file, or a declaration with no doc, simply contributes nothing.
  */
class ValueDocProcessor(rootsWithLayer: Seq[(Path, String)]) extends SingleFactProcessor[ValueDoc.Key] {

  override protected def generateSingleFact(key: ValueDoc.Key): CompilerIO[ValueDoc] =
    for {
      candidates  <- rootsWithLayer.toList.traverse { case (root, layer) =>
                       val file = root.resolve(key.vfqn.moduleName.toPath).toFile
                       getFactOrAbort(FileStat.Key(file)).map(stat => (file, layer, stat.lastModified.isDefined))
                     }
      present      = candidates.collect { case (file, layer, true) => (file, layer) }
      docsByLayer <- present.traverse { case (file, layer) =>
                       getFactOrAbort(SourceAST.Key(file.toURI))
                         .map(sourceAst => layer -> DocText.docOf(sourceAst.ast.value, key.vfqn.name))
                     }
      collected    = docsByLayer.collect { case (layer, Some(doc)) => layer -> doc }
      selected     = DocText.selectDoc(DocText.kindLabel(key.vfqn.name), key.vfqn.name.name, collected)
    } yield ValueDoc(key.vfqn, selected.doc, selected.warnings)
}
