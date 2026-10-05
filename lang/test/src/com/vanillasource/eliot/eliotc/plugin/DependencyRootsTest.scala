package com.vanillasource.eliot.eliotc.plugin

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scopt.OParser

import java.nio.file.Path

/** The two root lists a build tool passes: the roots being built, positionally or with `--path`, and the roots they are
  * built against, with `--dependency`. Every root is compiled alike; [[LangPlugin.currentRoots]] is what tells them apart.
  */
class DependencyRootsTest extends AnyFlatSpec with Matchers {

  "the command line" should "compile a dependency root like any other" in {
    parsed("p/src", "--dependency", "d/src").map(LangPlugin.allRoots) shouldBe Some(Seq(Path.of("p/src"), Path.of("d/src")))
  }

  it should "list the dependency roots in the order given" in {
    parsed("p/src", "--dependency", "d/src", "--dependency", "e/src").map(LangPlugin.dependencyRoots) shouldBe
      Some(Seq(Path.of("d/src"), Path.of("e/src")))
  }

  it should "leave the positional and `--path` roots current" in {
    parsed("p/src", "--dependency", "d/src", "--path", "q/src").map(LangPlugin.currentRoots) shouldBe
      Some(Seq(Path.of("p/src"), Path.of("q/src")))
  }

  it should "have no dependency roots when none is given" in {
    parsed("p/src", "--path", "q/src").map(LangPlugin.dependencyRoots) shouldBe Some(Seq.empty)
  }

  it should "keep a root given both ways current" in {
    parsed("p/src", "--dependency", "p/src").map(LangPlugin.currentRoots) shouldBe Some(Seq(Path.of("p/src"), Path.of("p/src")))
  }

  it should "still require a current root" in {
    parsed("--dependency", "d/src") shouldBe None
  }

  private def parsed(args: String*): Option[Configuration] =
    OParser.parse(LangPlugin().commandLineParser(), args, Configuration())
}
