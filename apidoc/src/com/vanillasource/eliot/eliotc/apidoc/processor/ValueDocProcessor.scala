package com.vanillasource.eliot.eliotc.apidoc.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.apidoc.build.DocText
import com.vanillasource.eliot.eliotc.apidoc.fact.ValueDoc
import com.vanillasource.eliot.eliotc.apidoc.render.SignatureRenderer
import com.vanillasource.eliot.eliotc.ast.fact.{AST, FunctionDefinition, SourceAST}
import com.vanillasource.eliot.eliotc.module.fact.{QualifiedName, Qualifier}
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.SingleFactProcessor
import com.vanillasource.eliot.eliotc.source.stat.FileStat

import java.nio.file.Path

/** Produces the documentation tile ([[ValueDoc]]: rendered signature + merged doc) for a fully-qualified name, purely
  * from the parsed ASTs — the same front-end source (the declarations and their `/** ... */` comments) the apidoc site
  * is built from, so the compiler proper (which drops docs at the core boundary) is never involved.
  *
  * It resolves the name's module to a file under every layer root — the compiler/runtime/user roots, each paired with a
  * layer label, captured at construction exactly as `PathScanner` captures its roots — reads each layer's [[SourceAST]],
  * then renders the definition through apidoc's own [[SignatureRenderer.forName]] (so hover shows the very tile the site
  * shows) and merges the doc comments through [[DocText]] (lowest layer wins, duplicates warned). A layer with no such
  * file contributes nothing.
  */
class ValueDocProcessor(rootsWithLayer: Seq[(Path, String)]) extends SingleFactProcessor[ValueDoc.Key] {

  override protected def generateSingleFact(key: ValueDoc.Key): CompilerIO[ValueDoc] =
    for {
      candidates <- rootsWithLayer.toList.traverse { case (root, layer) =>
                      val file = root.resolve(key.vfqn.moduleName.toPath).toFile
                      getFactOrAbort(FileStat.Key(file)).map(stat => (file, layer, stat.lastModified.isDefined))
                    }
      present     = candidates.collect { case (file, layer, true) => (file, layer) }
      layerAsts  <- present.traverse { case (file, layer) =>
                      getFactOrAbort(SourceAST.Key(file.toURI)).map(sourceAst => layer -> sourceAst.ast.value)
                    }
      docs        = layerAsts.collect { case (layer, ast) => (layer, DocText.docOf(ast, key.vfqn.name)) }
                      .collect { case (layer, Some(doc)) => layer -> doc }
      selected    = DocText.selectDoc(DocText.kindLabel(key.vfqn.name), key.vfqn.name.name, docs)
      signature   = renderSignature(key.vfqn.name, layerAsts.map(_._2))
    } yield ValueDoc(key.vfqn, signature, selected.doc, selected.warnings)

  /** The apidoc definition signature for `qn`, given the declarations across all present layers. Feeds
    * [[SignatureRenderer.forName]] exactly what it needs: the declarations named `qn`, the `data` of that name (for a
    * type constructor with no `type` declaration), and — for an ability method — the ability's own generic-parameter
    * count, so those shared parameters are dropped from the method's rendering.
    */
  private def renderSignature(qn: QualifiedName, asts: Seq[AST]): Option[String] = {
    val functions      = asts.flatMap(_.functionDefinitions)
    val matching       = functions.filter(_.name.value == qn)
    val matchingData   = if (qn.qualifier == Qualifier.Type) asts.flatMap(_.typeDefinitions).filter(_.name.value == qn.name) else Seq.empty
    val commonGenerics = qn.qualifier match {
      case Qualifier.Ability(ability) if qn.name != ability => markerGenericCount(functions, ability)
      case _                                                => 0
    }
    SignatureRenderer.forName(qn, matching, matchingData, commonGenerics)
  }

  /** The generic-parameter count of an ability's marker (the function whose name equals the ability), 0 if absent. */
  private def markerGenericCount(functions: Seq[FunctionDefinition], ability: String): Int =
    functions
      .find(_.name.value == QualifiedName(ability, Qualifier.Ability(ability)))
      .fold(0)(_.genericParameters.length)
}
