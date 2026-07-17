package com.vanillasource.eliot.eliotc.namedvalues

import cats.effect.IO
import com.vanillasource.eliot.eliotc.ProcessorTest
import com.vanillasource.eliot.eliotc.module.fact.ModuleName
import com.vanillasource.eliot.eliotc.platform.Platform
import com.vanillasource.eliot.eliotc.source.scan.{FilesystemMount, PoolModules, PoolModulesProcessor}
import com.vanillasource.eliot.eliotc.source.stat.FileStatProcessor

import java.nio.file.{Files, Path}

/** Exercises [[com.vanillasource.eliot.eliotc.source.scan.SourceMount.enumerate]] against a real filesystem tree,
  * driven through [[PoolModulesProcessor]] and the leaf [[FileStatProcessor]] (which the directory-listing dependencies
  * hang off).
  */
class PoolModulesProcessorTest
    extends ProcessorTest(
      FileStatProcessor(),
      PoolModulesProcessor(Seq.empty, Seq(FilesystemMount(PoolModulesProcessorTest.tempRoot)))
    ) {

  private def poolModules: IO[Set[ModuleName]] =
    runGeneratorWithFacts(Seq.empty, PoolModules.Key(Platform.Runtime)).map(_._1.get.modules)

  "pool modules" should "enumerate every .els file under the root as a module, including nested packages" in {
    poolModules.asserting(
      _ shouldBe Set(
        ModuleName(Seq.empty, "Top"),
        ModuleName(Seq("eliot", "lang"), "String"),
        ModuleName(Seq("eliot", "lang"), "Int")
      )
    )
  }
}

object PoolModulesProcessorTest {

  /** A fixed source tree: two nested-package modules, one top-level module, a non-`.els` file (excluded) and an empty
    * directory (walked, contributes nothing).
    */
  val tempRoot: Path = {
    val root = Files.createTempDirectory("pool-modules-test")
    Files.writeString(root.resolve("Top.els"), "type Top")
    Files.createDirectories(root.resolve("eliot/lang"))
    Files.writeString(root.resolve("eliot/lang/String.els"), "type String")
    Files.writeString(root.resolve("eliot/lang/Int.els"), "type Int")
    Files.writeString(root.resolve("notes.txt"), "not a module")
    Files.createDirectories(root.resolve("emptydir"))
    root
  }
}
