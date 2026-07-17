package com.vanillasource.eliot.eliotc.lsp.server

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

/** Parsing of the per-project `eliot.paths` file: runtime and compiler roots are kept in separate lists, relative paths
  * resolve against the file's own directory (so a Mill-style `src` works), absolutes pass through, and comments/blanks
  * and unknown directives are ignored. Absence of the file yields no configuration (the caller then falls back to
  * discovery).
  */
class WorkspacePathsTest extends AnyFlatSpec with Matchers {
  "eliot.paths" should "split runtime and compiler roots, resolving relative paths against the file directory" in {
    withPathsFile("""# base + platform layers, plus this project's own sources
                    |runtime  /opt/eliot/stdlib/eliot
                    |compiler /opt/eliot/stdlib/eliot-compiler
                    |
                    |runtime  src
                    |# a stray directive is skipped
                    |nonsense whatever""".stripMargin) { dir =>
      WorkspacePaths.load(Seq(dir)) shouldBe Some(
        WorkspacePaths(
          Seq(Path.of("/opt/eliot/stdlib/eliot"), dir.resolve("src").toAbsolutePath.normalize),
          Seq(Path.of("/opt/eliot/stdlib/eliot-compiler"))
        )
      )
    }
  }

  it should "be absent when no workspace root holds the file" in {
    withTempDir(dir => WorkspacePaths.load(Seq(dir)) shouldBe None)
  }

  private def withPathsFile[A](contents: String)(body: Path => A): A =
    withTempDir { dir =>
      Files.writeString(dir.resolve(WorkspacePaths.fileName), contents)
      body(dir)
    }

  private def withTempDir[A](body: Path => A): A = {
    val dir = Files.createTempDirectory("eliot-paths")
    try body(dir)
    finally Files.walk(dir).sorted(java.util.Comparator.reverseOrder()).iterator().asScala.foreach(Files.delete)
  }
}
