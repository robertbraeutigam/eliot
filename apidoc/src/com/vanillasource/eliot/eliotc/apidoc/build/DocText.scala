package com.vanillasource.eliot.eliotc.apidoc.build

import com.vanillasource.eliot.eliotc.ast.fact.AST
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, QualifiedName, Qualifier, ValueFQN}
import com.vanillasource.eliot.eliotc.source.content.Sourced

/** The single source of Eliot's *documentation* merge across platform layers.
  *
  * Both consumers of the doc pipeline go through this object, so they can never disagree: the apidoc HTML backend (via
  * [[DocModelBuilder]]) and the language server's hover (via the [[com.vanillasource.eliot.eliotc.apidoc.fact.ValueDoc]]
  * fact produced by [[com.vanillasource.eliot.eliotc.apidoc.processor.ValueDocProcessor]]). It only decides *which* doc
  * comment is canonical and how it is cleaned up; extracting the comment text from a file is the front end's job (the
  * `/** ... */` attached to each declaration's `doc` field).
  *
  * A name is documented once, on the declaration in its **lowest layer** (`lang` before `stdlib` before `compiler`
  * before `jvm`); any doc comment on a higher layer's copy of the same name is ignored and reported as a warning.
  */
object DocText {
  private val layerRank = Map("lang" -> 0, "stdlib" -> 1, "compiler" -> 2, "jvm" -> 3)

  /** The chosen, margin-stripped documentation for a name (or `None` if undocumented on every layer), together with the
    * "ignored duplicate doc" warnings for the layers whose copy was dropped.
    */
  case class Selected(doc: Option[String], warnings: Seq[String])

  /** Order layers canonically: lowest (introducing) layer first, unknown labels last, ties broken by name. */
  def sortLayers(layers: Seq[String]): Seq[String] =
    layers.distinct.sortBy(layer => (layerRank.getOrElse(layer, 4), layer))

  /** The doc comment attached to `qn` in a single file's AST, if any. A `Type`-qualified name may be introduced as a
    * `type` (a `FunctionDefinition` in the type namespace) or as a `data` declaration (whose bare-string name has no
    * qualifier), so both are consulted for the type namespace; every other namespace is a plain function declaration.
    */
  def docOf(ast: AST, qn: QualifiedName): Option[Sourced[String]] = {
    val fromFunction = ast.functionDefinitions.find(_.name.value == qn).flatMap(_.doc)
    val fromData     =
      if (qn.qualifier == Qualifier.Type) ast.typeDefinitions.find(_.name.value == qn.name).flatMap(_.doc)
      else None
    fromFunction.orElse(fromData)
  }

  /** A doc lookup computed purely from the in-memory layer files — the self-contained default source for
    * [[DocModelBuilder]] and its tests. The pipeline injects a `ValueDoc`-backed lookup instead, but both resolve to the
    * same answer because both extract with [[docOf]] and select with [[selectDoc]].
    */
  def fromLayerFiles(layerFiles: Seq[(ModuleName, String, AST)]): ValueFQN => Selected = {
    val byModule = layerFiles.groupBy(_._1)
    fqn => {
      val docs = byModule.getOrElse(fqn.moduleName, Seq.empty).flatMap { case (_, layer, ast) =>
        docOf(ast, fqn.name).map(layer -> _)
      }
      selectDoc(kindLabel(fqn.name), fqn.name.name, docs)
    }
  }

  /** A human label for a name's kind, used only in the ignored-duplicate warning text. */
  def kindLabel(qn: QualifiedName): String = qn.qualifier match {
    case Qualifier.Type                          => "type"
    case Qualifier.Meta                          => "meta"
    case Qualifier.Ability(ability) if qn.name == ability => "ability"
    case Qualifier.Ability(_)                    => "ability method"
    case _: Qualifier.AbilityImplementation      => "implementation"
    case Qualifier.Default                        => "def"
  }

  /** Choose the single canonical doc — the one on the lowest layer — and report every other layer's doc as ignored.
    *
    * `docsByLayer` is the layer→comment pairs that actually carry a doc (already filtered by [[docOf]]). The lowest
    * layer is where a name is introduced; a higher layer re-declaring it (a compiler-internal duplicate or a concrete
    * platform body) should not also document it. When it does, the higher-layer doc is dropped and a warning names both
    * layers so the duplicate can be removed (and drift caught).
    */
  def selectDoc(kind: String, name: String, docsByLayer: Seq[(String, Sourced[String])]): Selected =
    docsByLayer.sortBy { case (layer, _) => (layerRank.getOrElse(layer, 4), layer) } match {
      case Nil                                      => Selected(None, Seq.empty)
      case (selectedLayer, selectedDoc) +: ignored =>
        val doc      = Some(stripMargins(selectedDoc.value)).filter(_.nonEmpty)
        val warnings = ignored.map { case (ignoredLayer, _) =>
          s"Doc comment on $kind '$name' in layer '$ignoredLayer' is ignored; '$name' is already documented in layer '$selectedLayer'."
        }
        Selected(doc, warnings)
    }

  /** Strip the leading-`*` margins of a block documentation comment, Scaladoc-style, drop trailing whitespace on each
    * line (so a one-line block comment does not keep the space before its closing delimiter), and trim blank edge lines.
    */
  def stripMargins(raw: String): String = {
    val margin = "^\\s*\\* ?".r
    val lines  = raw.split("\n", -1).toVector.map { line =>
      val stripped = margin.findPrefixOf(line) match {
        case Some(prefix) => line.substring(prefix.length)
        case None         => if (line.trim.isEmpty) "" else line.replaceFirst("^ ", "")
      }
      stripped.replaceAll("[ \\t]+$", "")
    }
    lines.dropWhile(_.isEmpty).reverse.dropWhile(_.isEmpty).reverse.mkString("\n")
  }
}
