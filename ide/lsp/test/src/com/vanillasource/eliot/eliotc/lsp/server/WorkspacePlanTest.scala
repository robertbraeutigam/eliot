package com.vanillasource.eliot.eliotc.lsp.server

import cats.effect.{IO, Resource}
import cats.effect.testing.scalatest.AsyncIOSpec
import com.vanillasource.eliot.eliotc.lsp.buildtool.ProjectModel
import com.vanillasource.eliot.eliotc.lsp.buildtool.ProjectModel.{Resolved, Selection, Unresolved}
import com.vanillasource.eliot.eliotc.lsp.server.WorkspacePlan.Session
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

class WorkspacePlanTest extends AsyncFlatSpec with AsyncIOSpec with Matchers {
  private val project = Path.of("/p")
  private val eliot   = WorkspacePlan.compilerRepository

  private def resolved(name: String, eliotVersion: String): Resolved =
    Resolved(
      name,
      project.resolve(name).resolve("src"),
      Seq(project.resolve(name).resolve("src"), project.resolve("src")),
      Seq(Path.of(s"/c/eliot@$eliotVersion/stdlib/eliot/src")),
      Seq(Selection(eliot, eliotVersion), Selection("github.com/x/other", "v1.0"))
    )

  "a workspace plan" should "run one session per resolved package, over exactly the roots its build compiles" in {
    WorkspacePlan.fromModel(project, ProjectModel(Seq(resolved("test", "v0.6"))), None).sessions shouldBe Seq(
      Session(
        "test",
        Seq(Path.of("/p/test/src")),
        Seq(Path.of("/p/test/src"), Path.of("/p/src")),
        Seq(Path.of("/p/test/src"), Path.of("/p/src"), Path.of("/c/eliot@v0.6/stdlib/eliot/src")),
        Path.of("/p/.eliot-lsp/test"),
        Some(Seq(Path.of("/p/test/src"), Path.of("/p/src")))
      )
    )
  }

  it should "not run a session for a package the tool could not resolve, and say why" in {
    WorkspacePlan.fromModel(project, ProjectModel(Seq(Unresolved("test", "not fetched"))), None) shouldBe
      WorkspacePlan(Seq.empty, Seq("Package 'test' is not checked: not fetched"))
  }

  it should "warn once per compiler version that differs from the server's, naming the packages" in {
    WorkspacePlan
      .fromModel(project, ProjectModel(Seq(resolved("a", "v0.5"), resolved("b", "v0.5"), resolved("c", "v0.6"))), Some("v0.6"))
      .warnings shouldBe Seq(
      "'a', 'b' build with eliot v0.5, but this language server's compiler is v0.6: what it reports may differ from the build."
    )
  }

  it should "not warn about a package that builds with the server's own compiler version" in {
    WorkspacePlan.fromModel(project, ProjectModel(Seq(resolved("a", "v0.6"))), Some("v0.6")).warnings shouldBe empty
  }

  it should "not warn about versions when the server does not know its own" in {
    WorkspacePlan.fromModel(project, ProjectModel(Seq(resolved("a", "v0.5"))), None).warnings shouldBe empty
  }

  it should "tell a session's own files from those it checks and those it only mounts" in {
    val session = WorkspacePlan.fromModel(project, ProjectModel(Seq(resolved("test", "v0.6"))), None).sessions.head
    Seq("/p/test/src/A.els", "/p/src/B.els", "/c/eliot@v0.6/stdlib/eliot/src/C.els", "/q/D.els")
      .map(Path.of(_))
      .map(file => (session.owns(file), session.checks(file), session.mounts(file))) shouldBe
      Seq((true, true, true), (false, true, true), (false, false, true), (false, false, false))
  }

  it should "ask a project folder's tool, and plan from its answer" in {
    projectFolder.use { folder =>
      WorkspacePlan.of(Seq(folder), None, _ => IO.pure(Right(ProjectModel(Seq(Unresolved("x", "y"))))))
    }.asserting(_ shouldBe WorkspacePlan(Seq.empty, Seq("Package 'x' is not checked: y")))
  }

  it should "guess a project folder's roots when its tool gives no model, and say so" in {
    projectFolder.use { folder =>
      WorkspacePlan
        .of(Seq(folder), None, _ => IO.pure(Left("exited with 1")))
        .map(plan => (plan.sessions.map(session => (session.name, session.roots, session.checkedRoots)), plan.warnings))
        .map(_ -> folder)
    }.asserting { case ((sessions, warnings), folder) =>
      (sessions, warnings) shouldBe (
        Seq(("workspace", Seq(folder.resolve("src")), None)),
        Seq(s"Could not read the project model of $folder, so its source roots are guessed: exited with 1")
      )
    }
  }

  it should "guess a folder's roots without asking when it is not a project" in {
    projectFolder.use { folder =>
      IO.blocking(Files.delete(folder.resolve("eliotw"))) >>
        WorkspacePlan
          .of(Seq(folder), None, _ => IO.raiseError(new IllegalStateException("asked")))
          .map(_.sessions.map(_.name))
    }.asserting(_ shouldBe Seq("workspace"))
  }

  it should "plan nothing for no folders" in {
    WorkspacePlan.of(Seq.empty, None, _ => IO.raiseError(new IllegalStateException("asked"))).asserting(
      _ shouldBe WorkspacePlan(Seq.empty, Seq.empty)
    )
  }

  /** A folder with a descriptor, a wrapper and one source file under `src`. */
  private def projectFolder: Resource[IO, Path] =
    Resource.make(IO.blocking {
      val folder = Files.createTempDirectory("eliot-lsp-plan").toRealPath()
      Files.writeString(folder.resolve("eliot.pkg"), "launcher v0.6\n")
      Files.writeString(folder.resolve("eliotw"), "#!/bin/sh\n")
      Files.createDirectories(folder.resolve("src"))
      Files.writeString(folder.resolve("src").resolve("A.els"), "def a: String = \"a\"\n")
      folder
    })(folder =>
      IO.blocking(Files.walk(folder).sorted(java.util.Comparator.reverseOrder()).iterator().asScala.foreach(Files.delete))
    )
}
