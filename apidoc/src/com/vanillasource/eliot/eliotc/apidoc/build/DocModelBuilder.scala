package com.vanillasource.eliot.eliotc.apidoc.build

import com.vanillasource.eliot.eliotc.apidoc.model.{DocItem, DocModule}
import com.vanillasource.eliot.eliotc.apidoc.render.SignatureRenderer
import com.vanillasource.eliot.eliotc.ast.fact.{AST, DataDefinition, FunctionDefinition}
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, Qualifier}

/** Builds the presentation [[DocModule]] model from the per-file parsed ASTs, each tagged with the module it belongs to
  * and the layer (source root) it was found under.
  *
  * This is the apidoc-specific merge of Eliot's layering. Unlike the compiler's unifier — which *picks* one declaration
  * and rejects conflicts — this view keeps them all: declarations of the same name across layers collapse into one item
  * showing the canonical (abstract) signature, every layer that declares it, and the layers that implement it. The three
  * shapes are recovered from the qualifier the front end assigned: a `data`/`type` becomes a type-like item (abstract
  * `type IO[A]` merged with the concrete `data IO[A](..)` of a platform layer), an ability marker plus its methods
  * becomes an ability item (with its implementations gathered workspace-wide), and a plain `def` becomes a value item.
  */
object DocModelBuilder {
  private val layerRank = Map("lang" -> 0, "stdlib" -> 1, "compiler" -> 2, "jvm" -> 3)

  def build(layerFiles: Seq[(ModuleName, String, AST)]): Seq[DocModule] = {
    val implementationsByAbility: Map[String, Seq[(String, FunctionDefinition)]] =
      layerFiles
        .flatMap { case (_, layer, ast) => ast.functionDefinitions.flatMap(fn => implMarkerAbility(fn).map(_ -> (layer, fn))) }
        .groupBy(_._1)
        .view
        .mapValues(_.map(_._2))
        .toMap

    layerFiles
      .groupBy(_._1)
      .toSeq
      .sortBy { case (moduleName, _) => (moduleName.packages.mkString("."), moduleName.name) }
      .map { case (moduleName, files) =>
        buildModule(moduleName, files.map { case (_, layer, ast) => (layer, ast) }, implementationsByAbility)
      }
  }

  private def buildModule(
      moduleName: ModuleName,
      files: Seq[(String, AST)],
      implementationsByAbility: Map[String, Seq[(String, FunctionDefinition)]]
  ): DocModule = {
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

    DocModule(moduleName, None, typeItems ++ abilityItems ++ valueItems)
  }

  private def buildValueItem(name: String, decls: Seq[(String, FunctionDefinition)]): DocItem = {
    val abstractDecl = decls.find(_._2.body.isEmpty)
    DocItem(
      name = name,
      kind = DocItem.Kind.Value,
      signature = SignatureRenderer.function(abstractDecl.getOrElse(decls.head)._2),
      doc = preferredDoc(abstractDecl.flatMap(_._2.doc), decls.flatMap(_._2.doc)),
      layers = sortLayers(decls.map(_._1)),
      implementedOn = sortLayers(decls.filter(_._2.body.isDefined).map(_._1))
    )
  }

  private def buildTypeItem(
      name: String,
      typeDecls: Seq[(String, FunctionDefinition)],
      dataDecls: Seq[(String, DataDefinition)]
  ): DocItem = {
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

    DocItem(
      name = name,
      kind = DocItem.Kind.TypeLike,
      signature = primarySignature,
      doc = preferredDoc(
        abstractType.flatMap(_._2.doc),
        typeDecls.flatMap(_._2.doc) ++ dataDecls.flatMap(_._2.doc)
      ),
      layers = sortLayers(typeDecls.map(_._1) ++ dataDecls.map(_._1)),
      implementedOn = sortLayers(concreteTypes.map(_._1) ++ dataDecls.map(_._1)),
      members = members
    )
  }

  private def buildAbilityItem(
      name: String,
      markers: Seq[(String, FunctionDefinition)],
      methods: Seq[(String, FunctionDefinition)],
      implementations: Seq[(String, FunctionDefinition)]
  ): DocItem = {
    val commonParameters = markers.head._2.genericParameters

    val methodMembers = methods
      .groupBy(_._2.name.value.name)
      .toSeq
      .sortBy(_._1)
      .map { case (_, group) =>
        val abstractMethod = group.find(_._2.body.isEmpty).getOrElse(group.head)._2
        // Drop the ability's common generics (the front end prepends them to every method) so the method reads as written.
        val written        = abstractMethod.copy(genericParameters = abstractMethod.genericParameters.drop(commonParameters.length))
        DocItem.Member(SignatureRenderer.function(written), preferredDoc(None, group.flatMap(_._2.doc)))
      }

    val implementationItems = implementations
      .map { case (layer, marker) => SignatureRenderer.implementation(name, marker) -> layer }
      .groupBy(_._1)
      .toSeq
      .sortBy(_._1)
      .map { case (signature, group) => DocItem.Implementation(signature, sortLayers(group.map(_._2)), None) }

    DocItem(
      name = name,
      kind = DocItem.Kind.Ability,
      signature = SignatureRenderer.abilityHeader(name, commonParameters),
      doc = preferredDoc(markers.flatMap(_._2.doc).headOption, markers.flatMap(_._2.doc)),
      layers = sortLayers(markers.map(_._1) ++ methods.map(_._1)),
      members = methodMembers,
      implementations = implementationItems
    )
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

  /** Prefer the abstract declaration's doc, else the first available; strip the comment margins and drop if empty. */
  private def preferredDoc(
      preferred: Option[com.vanillasource.eliot.eliotc.source.content.Sourced[String]],
      all: Seq[com.vanillasource.eliot.eliotc.source.content.Sourced[String]]
  ): Option[String] =
    preferred.orElse(all.headOption).map(s => stripMargins(s.value)).filter(_.nonEmpty)

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
