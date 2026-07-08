package com.vanillasource.eliot.eliotc.lsp.server

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.Path

/** Exercises the workspace source-root discovery against the compiler repo itself (handed in via `ELIOT_REPO_ROOT`, see
  * `build.mill`). The repo is the hard case: its own folder is literally named `eliot`, it holds several `eliot` layer
  * roots plus an `eliot-compiler` overlay and many Scala `.../vanillasource/eliot` package directories.
  */
class SourceRootDiscoveryTest extends AnyFlatSpec with Matchers {
  private val repoRoot                         =
    Path.of(Option(System.getenv("ELIOT_REPO_ROOT")).getOrElse(System.getProperty("user.dir")))
  private def moduleRoot(module: String, root: String): Path = repoRoot.resolve(module).resolve(root)

  "source-root discovery" should "recover exactly the layer and application roots under the repo folder" in {
    SourceRootDiscovery.discover(Seq(repoRoot)).toSet shouldBe Set(
      moduleRoot("lang", "eliot"),
      moduleRoot("stdlib", "eliot"),
      moduleRoot("jvm", "eliot"),
      moduleRoot("examples", "src")
    )
  }

  it should "not descend an application root, so the example files' own directory is the single root" in {
    SourceRootDiscovery.discover(Seq(repoRoot.resolve("examples"))).toSet shouldBe Set(repoRoot.resolve("examples").resolve("src"))
  }
}
