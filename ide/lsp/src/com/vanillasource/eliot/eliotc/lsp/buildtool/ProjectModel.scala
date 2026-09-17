package com.vanillasource.eliot.eliotc.lsp.buildtool

import com.google.gson.{JsonElement, JsonObject, JsonParser}

import java.nio.file.Path
import scala.jdk.CollectionConverters.*
import scala.util.Try

/** What the build tool says a project's packages resolve to — the answer to `eliot --project-model` (eliot-build's
  * `docs/build-system.md`, "IDE integration").
  *
  * One entry per package the project's `eliot.pkg` declares, each resolved on its own: a package is either
  * [[ProjectModel.Resolved]], with the directories a build of it compiles from, or [[ProjectModel.Unresolved]], with the
  * reason the tool could not resolve it from what is already fetched.
  *
  * {{{
  *   {"packages": [
  *     {"name": "root", "ownRoot": "/p/src", "roots": ["/p/src"], "dependencyRoots": ["/p/target/cache/…/stdlib/eliot/src"],
  *      "selections": [{"repository": "github.com/robertbraeutigam/eliot", "version": "v0.6", "packages": ["stdlib"]}]},
  *     {"name": "test", "problem": "github.com/eliotlang/eliot-test is not fetched …"}
  *   ]}
  * }}}
  */
final case class ProjectModel(packages: Seq[ProjectModel.Package])

object ProjectModel {

  /** One package of the project. */
  sealed trait Package {
    def name: String
  }

  /** A package the tool resolved.
    *
    * @param ownRoot
    *   the source root the package itself declares — a file under it belongs to this package.
    * @param roots
    *   the project's roots a build of the package opens, its own and its siblings'.
    * @param dependencyRoots
    *   the roots the package's closure mounted from its dependencies.
    * @param selections
    *   the version selected for each repository in the closure.
    */
  final case class Resolved(
      name: String,
      ownRoot: Path,
      roots: Seq[Path],
      dependencyRoots: Seq[Path],
      selections: Seq[Selection]
  ) extends Package {

    /** Every root a build of this package compiles from, in the tool's order. */
    def allRoots: Seq[Path] = (roots ++ dependencyRoots).distinct
  }

  /** A package the tool could not resolve from what is on this machine, and why. */
  final case class Unresolved(name: String, problem: String) extends Package

  /** The version a closure selected for one repository, e.g. `github.com/robertbraeutigam/eliot` at `v0.6`. */
  final case class Selection(repository: String, version: String)

  /** Read the tool's JSON output, or say why it is not a project model. */
  def parse(json: String): Either[String, ProjectModel] =
    Try(JsonParser.parseString(json).getAsJsonObject)
      .flatMap(root => Try(ProjectModel(array(root, "packages").map(element => packageOf(element.getAsJsonObject)))))
      .toEither
      .left
      .map(error => s"not a project model: ${error.getMessage}")

  private def packageOf(entry: JsonObject): Package = {
    val name = string(entry, "name")
    if (entry.has("problem")) Unresolved(name, string(entry, "problem"))
    else
      Resolved(
        name,
        Path.of(string(entry, "ownRoot")),
        paths(entry, "roots"),
        paths(entry, "dependencyRoots"),
        array(entry, "selections").map(_.getAsJsonObject).map(selectionOf)
      )
  }

  private def selectionOf(entry: JsonObject): Selection = Selection(string(entry, "repository"), string(entry, "version"))

  private def paths(entry: JsonObject, field: String): Seq[Path] = array(entry, field).map(element => Path.of(element.getAsString))

  private def array(entry: JsonObject, field: String): Seq[JsonElement] =
    Option(entry.getAsJsonArray(field)).getOrElse(throw new IllegalArgumentException(s"no '$field'")).asScala.toSeq

  private def string(entry: JsonObject, field: String): String =
    Option(entry.get(field)).getOrElse(throw new IllegalArgumentException(s"no '$field'")).getAsString
}
