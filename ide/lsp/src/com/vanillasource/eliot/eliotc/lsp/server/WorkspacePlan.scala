package com.vanillasource.eliot.eliotc.lsp.server

import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.lsp.buildtool.{ProjectModel, ProjectModelQuery}

import java.nio.file.Path

/** Which compile sessions the language server runs over the editor's workspace folders, and what it has to warn the
  * user about while deciding.
  *
  * **One session per package.** A folder that is a build-tool project ([[ProjectModelQuery.isProject]]) is asked for its
  * [[ProjectModel]], and every package in it becomes a session of its own, compiling exactly the roots a build of that
  * package would. A union would be a build nobody runs: two packages may close over different platforms (whose layers
  * collide in the merge) and over different versions of one repository.
  *
  * A folder that is not a project, or whose tool could not answer, falls back to [[SourceRootDiscovery]]: all such
  * folders share one guessed session, which is how the server behaved before there was a tool to ask.
  *
  * @param sessions
  *   the sessions to start, in the order a file's owner is looked for.
  * @param warnings
  *   what the user should be told: a package that is not checked, a tool that did not answer, a compiler version that
  *   differs from the one a package builds with.
  */
final case class WorkspacePlan(sessions: Seq[WorkspacePlan.Session], warnings: Seq[String])

object WorkspacePlan {

  /** The repository this server's compiler is released from — how a selection naming the compiler is recognised. */
  val compilerRepository: String = "github.com/robertbraeutigam/eliot"

  /** The name of the one guessed session covering every folder the build tool does not describe. */
  val guessedSessionName: String = "workspace"

  /** One compile session.
    *
    * @param name
    *   the package it compiles, for messages.
    * @param ownRoots
    *   the roots the package itself declares; a file under one belongs to this session.
    * @param currentRoots
    *   the project's roots this session checks: its own and its siblings'. Every name under them is diagnosed.
    * @param roots
    *   every root the session compiles from, dependencies included.
    * @param target
    *   the session's own cache directory.
    * @param checkedRoots
    *   the roots to diagnose, when the package model says which they are; `None` for the guessed session, which keeps the
    *   driver's own recognition of library modules.
    */
  final case class Session(
      name: String,
      ownRoots: Seq[Path],
      currentRoots: Seq[Path],
      roots: Seq[Path],
      target: Path,
      checkedRoots: Option[Seq[Path]]
  ) {

    /** Whether `file` belongs to this session's package. */
    def owns(file: Path): Boolean = ownRoots.exists(file.startsWith)

    /** Whether this session checks `file` as one of the project's own sources. */
    def checks(file: Path): Boolean = currentRoots.exists(file.startsWith)

    /** Whether `file` is anywhere on this session's path. */
    def mounts(file: Path): Boolean = roots.exists(file.startsWith)
  }

  /** Plan the sessions for `workspaceRoots`, asking each project folder's tool through `query`. `serverVersion` is this
    * server's own eliot version, when it knows one.
    */
  def of(
      workspaceRoots: Seq[Path],
      serverVersion: Option[String],
      query: Path => IO[Either[String, ProjectModel]]
  ): IO[WorkspacePlan] =
    workspaceRoots
      .traverse { root =>
        if (ProjectModelQuery.isProject(root)) query(root).map(root -> Some(_)) else IO.pure(root -> None)
      }
      .flatMap { answers =>
        val modelled = answers.collect { case (root, Some(Right(model))) => fromModel(root, model, serverVersion) }
        val failed   = answers.collect { case (root, Some(Left(problem))) =>
          s"Could not read the project model of $root, so its source roots are guessed: $problem"
        }
        val guessed  = answers.collect { case (root, None) => root } ++
          answers.collect { case (root, Some(Left(_))) => root }
        IO.blocking(guessedPlan(guessed)).map { guessedPart =>
          WorkspacePlan(
            modelled.flatMap(_.sessions) ++ guessedPart.sessions,
            modelled.flatMap(_.warnings) ++ failed
          )
        }
      }

  /** The sessions `model` describes for the project at `projectRoot`: one per resolved package, and a warning for each
    * package that is not, and for each compiler version that differs from `serverVersion`.
    */
  def fromModel(projectRoot: Path, model: ProjectModel, serverVersion: Option[String]): WorkspacePlan = {
    val resolved   = model.packages.collect { case pkg: ProjectModel.Resolved => pkg }
    val unresolved = model.packages.collect { case ProjectModel.Unresolved(name, problem) =>
      s"Package '$name' is not checked: $problem"
    }
    WorkspacePlan(
      resolved.map(pkg =>
        Session(
          pkg.name,
          Seq(pkg.ownRoot),
          pkg.roots,
          pkg.allRoots,
          projectRoot.resolve(".eliot-lsp").resolve(pkg.name),
          Some(pkg.roots)
        )
      ),
      unresolved ++ versionWarnings(resolved, serverVersion)
    )
  }

  /** The one guessed session over `folders`, or nothing when there are none. */
  def guessedPlan(folders: Seq[Path]): WorkspacePlan =
    if (folders.isEmpty) WorkspacePlan(Seq.empty, Seq.empty)
    else {
      val roots = SourceRootDiscovery.discover(folders)
      WorkspacePlan(
        Seq(Session(guessedSessionName, roots, roots, roots, folders.head.resolve(".eliot-lsp"), None)),
        Seq.empty
      )
    }

  /** One warning per compiler version the packages select that is not this server's. Silent when the server does not
    * know its own version (a development run, not a packaged server).
    */
  private def versionWarnings(packages: Seq[ProjectModel.Resolved], serverVersion: Option[String]): Seq[String] =
    serverVersion.toSeq.flatMap { own =>
      packages
        .flatMap(pkg => pkg.selections.filter(_.repository == compilerRepository).map(_.version -> pkg.name))
        .filterNot(_._1 == own)
        .groupMap(_._1)(_._2)
        .toSeq
        .sortBy(_._1)
        .map { (version, names) =>
          s"${names.map(name => s"'$name'").mkString(", ")} ${if (names.size == 1) "builds" else "build"} with eliot " +
            s"$version, but this language server's compiler is $own: what it reports may differ from the build."
        }
    }
}
