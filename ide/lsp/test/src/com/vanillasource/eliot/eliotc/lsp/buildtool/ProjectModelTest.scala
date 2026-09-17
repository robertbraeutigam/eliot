package com.vanillasource.eliot.eliotc.lsp.buildtool

import com.vanillasource.eliot.eliotc.lsp.buildtool.ProjectModel.{Resolved, Selection, Unresolved}
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.Path

class ProjectModelTest extends AnyFlatSpec with Matchers with EitherValues {
  private val resolvedJson =
    """{"packages": [
      |  {"name": "root", "ownRoot": "/p/src", "roots": ["/p/src"], "dependencyRoots": ["/c/lang/src", "/c/stdlib/src"],
      |   "selections": [{"repository": "github.com/robertbraeutigam/eliot", "version": "v0.6", "packages": ["stdlib", "lang"]}]}
      |]}""".stripMargin

  "a project model" should "read a resolved package with its roots and selections" in {
    ProjectModel.parse(resolvedJson).value shouldBe ProjectModel(
      Seq(
        Resolved(
          "root",
          Path.of("/p/src"),
          Seq(Path.of("/p/src")),
          Seq(Path.of("/c/lang/src"), Path.of("/c/stdlib/src")),
          Seq(Selection("github.com/robertbraeutigam/eliot", "v0.6"))
        )
      )
    )
  }

  it should "read a package the tool could not resolve, with the reason" in {
    ProjectModel.parse("""{"packages": [{"name": "test", "problem": "not fetched"}]}""").value shouldBe
      ProjectModel(Seq(Unresolved("test", "not fetched")))
  }

  it should "read a project with no packages" in {
    ProjectModel.parse("{\"packages\": [\n\n]}").value shouldBe ProjectModel(Seq.empty)
  }

  it should "list a resolved package's own and dependency roots together, without repeats" in {
    Resolved("r", Path.of("/p/src"), Seq(Path.of("/p/src")), Seq(Path.of("/d"), Path.of("/p/src")), Seq.empty).allRoots shouldBe
      Seq(Path.of("/p/src"), Path.of("/d"))
  }

  it should "refuse output that is not JSON" in {
    ProjectModel.parse("usage: eliot <package>").left.value should startWith("not a project model")
  }

  it should "refuse a package missing a field" in {
    ProjectModel.parse("""{"packages": [{"name": "root", "roots": []}]}""").left.value should include("ownRoot")
  }
}
