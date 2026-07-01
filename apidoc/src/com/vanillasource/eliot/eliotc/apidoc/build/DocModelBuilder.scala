package com.vanillasource.eliot.eliotc.apidoc.build

import com.vanillasource.eliot.eliotc.apidoc.model.{DocItem, DocModule}
import com.vanillasource.eliot.eliotc.apidoc.render.SignatureRenderer
import com.vanillasource.eliot.eliotc.ast.fact.{AST, DataDefinition, FunctionDefinition}
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, Qualifier}
import com.vanillasource.eliot.eliotc.source.content.Sourced

/** Builds the presentation [[DocModule]] model from the per-file parsed ASTs, each tagged with the module it belongs to
  * and the layer (source root) it was found under.
  *
  * This is the apidoc-specific merge of Eliot's layering. Unlike the compiler's unifier — which *picks* one declaration
  * and rejects conflicts — this view keeps them all: declarations of the same name across layers collapse into one item
  * showing the canonical (abstract) signature, every layer that declares it, and the layers that implement it. The three
  * shapes are recovered from the qualifier the front end assigned: a `data`/`type` becomes a type-like item (abstract
  * `type IO[A]` merged with the concrete `data IO[A](..)` of a platform layer), an ability marker plus its methods
  * becomes an ability item (with its implementations gathered workspace-wide), and a plain `def` becomes a value item.
  *
  * Documentation is single-sourced: a name is documented once, on the declaration in its **lowest layer** (`lang` before
  * `stdlib` before `compiler`/`jvm`), and any doc comment on a higher layer's copy of the same name is **ignored** and
  * reported as a [[Result.warnings]] entry. So the canonical doc lives where the name is introduced, and a duplicated
  * (drifting) copy in a repeating layer is surfaced rather than silently dropped.
  */
object DocModelBuilder {
  private val layerRank = Map("lang" -> 0, "stdlib" -> 1, "compiler" -> 2, "jvm" -> 3)

  /** The built documentation model plus any layering diagnostics (ignored duplicate doc comments). */
  case class Result(modules: Seq[DocModule], warnings: Seq[String])

  def build(layerFiles: Seq[(ModuleName, String, AST)]): Result = {
    val implementationsByAbility: Map[String, Seq[(String, FunctionDefinition)]] =
      layerFiles
        .flatMap { case (_, layer, ast) => ast.functionDefinitions.flatMap(fn => implMarkerAbility(fn).map(_ -> (layer, fn))) }
        .groupBy(_._1)
        .view
        .mapValues(_.map(_._2))
        .toMap

    val builtModules = layerFiles
      .groupBy(_._1)
      .toSeq
      .sortBy { case (moduleName, _) => (moduleName.packages.mkString("."), moduleName.name) }
      .map { case (moduleName, files) =>
        buildModule(moduleName, files.map { case (_, layer, ast) => (layer, ast) }, implementationsByAbility)
      }

    Result(builtModules.map(_._1), builtModules.flatMap(_._2))
  }

  private def buildModule(
      moduleName: ModuleName,
      files: Seq[(String, AST)],
      implementationsByAbility: Map[String, Seq[(String, FunctionDefinition)]]
  ): (DocModule, Seq[String]) = {
    val taggedFunctions = files.flatMap { case (layer, ast) => ast.functionDefinitions.map(layer -> _) }
    val taggedData      = files.flatMap { case (layer, ast) => ast.typeDefinitions.map(layer -> _) }

    val values   = taggedFunctions.filter(_._2.name.value.qualifier == Qualifier.Default)
    val types    = taggedFunctions.filter(_._2.name.value.qualifier == Qualifier.Type)
    val abilityMethods = taggedFunctions.filter(t => abilityMethodOf(t._2).isDefined)

    val typeItems = (types.map(_._2.name.value.name) ++ taggedData.map(_._2.name.value)).distinct.sorted.map { name =>
      buildTypeItem(name, types.filter(_._2.name.value.name == name), taggedData.filter(_._2.name.value == name))
    }

    val abilityItems = taggedFunctions.flatMap(t => abilityMarkerOf(t._2)).distinct.sorted.map { name =>
      buildAbilityItem(
        name,
        taggedFunctions.filter(t => abilityMarkerOf(t._2).contains(name)),
        abilityMethods.filter(t => abilityMethodOf(t._2).contains(name)),
        implementationsByAbility.getOrElse(name, Seq.empty)
      )
    }

    val valueItems =
      values.map(_._2.name.value.name).distinct.sorted.map(name => buildValueItem(name, values.filter(_._2.name.value.name == name)))

    val all = typeItems ++ abilityItems ++ valueItems
    (DocModule(moduleName, None, all.map(_._1)), all.flatMap(_._2))
  }

  private def buildValueItem(name: String, decls: Seq[(String, FunctionDefinition)]): (DocItem, Seq[String]) = {
    val abstractDecl        = decls.find(_._2.body.isEmpty)
    val (doc, docWarnings)  = selectDoc("def", name, layerDocs(decls.map { case (l, fn) => (l, fn.doc) }))
    val item                = DocItem(
      name = name,
      kind = DocItem.Kind.Value,
      signature = SignatureRenderer.function(abstractDecl.getOrElse(decls.head)._2),
      doc = doc,
      layers = sortLayers(decls.map(_._1)),
      implementedOn = sortLayers(decls.filter(_._2.body.isDefined).map(_._1))
    )
    (item, docWarnings)
  }

  private def buildTypeItem(
      name: String,
      typeDecls: Seq[(String, FunctionDefinition)],
      dataDecls: Seq[(String, DataDefinition)]
  ): (DocItem, Seq[String]) = {
    val abstractType   = typeDecls.find(_._2.body.isEmpty)
    val concreteTypes  = typeDecls.filter(_._2.body.isDefined)
    val primarySignature = abstractType
      .map(t => SignatureRenderer.function(t._2))
      .orElse(typeDecls.headOption.map(t => SignatureRenderer.function(t._2)))
      .orElse(dataDecls.headOption.map(t => SignatureRenderer.data(t._2)))
      .getOrElse(name)

    val concreteDefinitions =
      dataDecls.map { case (layer, dd) => SignatureRenderer.data(dd) -> layer } ++
        concreteTypes.map { case (layer, fn) => SignatureRenderer.function(fn) -> layer }

    val members = concreteDefinitions
      .filterNot(_._1 == primarySignature)
      .groupBy(_._1)
      .toSeq
      .sortBy(_._1)
      .map { case (signature, group) => DocItem.Member(signature, None, Some(sortLayers(group.map(_._2)).mkString(", "))) }

    val (doc, docWarnings) = selectDoc(
      "type",
      name,
      layerDocs(typeDecls.map { case (l, fn) => (l, fn.doc) } ++ dataDecls.map { case (l, dd) => (l, dd.doc) })
    )

    val item = DocItem(
      name = name,
      kind = DocItem.Kind.TypeLike,
      signature = primarySignature,
      doc = doc,
      layers = sortLayers(typeDecls.map(_._1) ++ dataDecls.map(_._1)),
      implementedOn = sortLayers(concreteTypes.map(_._1) ++ dataDecls.map(_._1)),
      members = members
    )
    (item, docWarnings)
  }

  private def buildAbilityItem(
      name: String,
      markers: Seq[(String, FunctionDefinition)],
      methods: Seq[(String, FunctionDefinition)],
      implementations: Seq[(String, FunctionDefinition)]
  ): (DocItem, Seq[String]) = {
    val commonParameters = markers.head._2.genericParameters

    val methodMembersWithWarnings = methods
      .groupBy(_._2.name.value.name)
      .toSeq
      .sortBy(_._1)
      .map { case (methodName, group) =>
        val abstractMethod = group.find(_._2.body.isEmpty).getOrElse(group.head)._2
        // Drop the ability's common generics (the front end prepends them to every method) so the method reads as written.
        val written        = abstractMethod.copy(genericParameters = abstractMethod.genericParameters.drop(commonParameters.length))
        val (doc, warns)   = selectDoc("ability method", methodName, layerDocs(group.map { case (l, fn) => (l, fn.doc) }))
        (DocItem.Member(SignatureRenderer.function(written), doc), warns)
      }

    val implementationItems = implementations
      .map { case (layer, marker) => SignatureRenderer.implementation(name, marker) -> layer }
      .groupBy(_._1)
      .toSeq
      .sortBy(_._1)
      .map { case (signature, group) => DocItem.Implementation(signature, sortLayers(group.map(_._2)), None) }

    val (doc, docWarnings) = selectDoc("ability", name, layerDocs(markers.map { case (l, fn) => (l, fn.doc) }))

    val item = DocItem(
      name = name,
      kind = DocItem.Kind.Ability,
      signature = SignatureRenderer.abilityHeader(name, commonParameters),
      doc = doc,
      layers = sortLayers(markers.map(_._1) ++ methods.map(_._1)),
      members = methodMembersWithWarnings.map(_._1),
      implementations = implementationItems
    )
    (item, docWarnings ++ methodMembersWithWarnings.flatMap(_._2))
  }

  private def abilityMarkerOf(fn: FunctionDefinition): Option[String] = fn.name.value.qualifier match {
    case Qualifier.Ability(abilityName) if fn.name.value.name == abilityName => Some(abilityName)
    case _                                                                   => None
  }

  private def abilityMethodOf(fn: FunctionDefinition): Option[String] = fn.name.value.qualifier match {
    case Qualifier.Ability(abilityName) if fn.name.value.name != abilityName => Some(abilityName)
    case _                                                                   => None
  }

  private def implMarkerAbility(fn: FunctionDefinition): Option[String] = fn.name.value.qualifier match {
    case Qualifier.AbilityImplementation(abilityName, _) if fn.name.value.name == abilityName.value => Some(abilityName.value)
    case _                                                                                          => None
  }

  /** Keep only the layers that actually carry a doc comment, pairing each with its raw text. */
  private def layerDocs(docs: Seq[(String, Option[Sourced[String]])]): Seq[(String, Sourced[String])] =
    docs.collect { case (layer, Some(doc)) => (layer, doc) }

  /** Choose the single canonical doc — the one on the lowest layer — and report every other layer's doc as ignored.
    *
    * The lowest layer is where a name is introduced; a higher layer re-declaring it (a compiler-internal duplicate or a
    * concrete platform body) should not also document it. When it does, the higher-layer doc is dropped and a warning
    * names both layers so the duplicate can be removed (and drift caught).
    */
  private def selectDoc(kind: String, name: String, docsByLayer: Seq[(String, Sourced[String])]): (Option[String], Seq[String]) =
    docsByLayer.sortBy { case (layer, _) => (layerRank.getOrElse(layer, 4), layer) } match {
      case Nil                            => (None, Seq.empty)
      case (selectedLayer, selectedDoc) +: ignored =>
        val doc      = Some(stripMargins(selectedDoc.value)).filter(_.nonEmpty)
        val warnings = ignored.map { case (ignoredLayer, _) =>
          s"Doc comment on $kind '$name' in layer '$ignoredLayer' is ignored; '$name' is already documented in layer '$selectedLayer'."
        }
        (doc, warnings)
    }

  /** Strip the leading-`*` margins of a block documentation comment, Scaladoc-style, and trim blank edge lines. */
  private def stripMargins(raw: String): String = {
    val margin = "^\\s*\\* ?".r
    val lines  = raw.split("\n", -1).toVector.map { line =>
      margin.findPrefixOf(line) match {
        case Some(prefix) => line.substring(prefix.length)
        case None         => if (line.trim.isEmpty) "" else line.replaceFirst("^ ", "")
      }
    }
    lines.dropWhile(_.trim.isEmpty).reverse.dropWhile(_.trim.isEmpty).reverse.mkString("\n")
  }

  private def sortLayers(layers: Seq[String]): Seq[String] =
    layers.distinct.sortBy(layer => (layerRank.getOrElse(layer, 4), layer))
}
