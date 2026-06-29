package com.vanillasource.eliot.eliotc.termination

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.io.File
import scala.io.Source

/** M0 precondition #3 (purity — the store / Landin's knot): Eliot ships **no general user-facing mutable cell**. An
  * unrestricted cell would let a program tie a recursion knot through the heap (store a function that reads and calls
  * the cell, then backpatch) with no syntactic cycle and no recursive type — defeating the no-recursion guarantee the
  * termination model rests on (see the "Total by Default" cornerstone in `.claude/CLAUDE.md`).
  *
  * This is the durable guard recording that confirmation: it scans every `.els` source of the language layers for a
  * declaration of a mutable-cell primitive and fails if one ever appears. A mutable cell can only enter the language as
  * a native (the pure fragment cannot express one), so it would have to be declared in `.els` here.
  *
  * If this test ever fails because a cell was intentionally added: do **not** just widen the blocklist. Revisit the
  * termination model first — per the graceful-fallback note, a cell holding a *function* must be priced `{Inf}` (you
  * cannot optimistically infer termination for mutable callable contents); a cell holding plain data is fine.
  */
class PurityGuardTest extends AnyFlatSpec with Matchers {

  /** The canonical mutable-reference vocabulary. A declaration (`type`/`data`/`def`) of any of these names denotes a
    * general mutable cell — the construct precondition #3 forbids.
    */
  private val forbiddenCellNames: Set[String] =
    Set("Ref", "IORef", "STRef", "MutableRef", "MutVar", "Cell", "MutableCell", "AtomicRef", "AtomicReference") ++
      Set("newRef", "readRef", "writeRef", "modifyRef", "newIORef", "newCell", "readCell", "writeCell", "newMutable")

  /** A `type X`, `data X`, or `def x` declaration head — captures the declared name in group 1. */
  private val declarationPattern = """(?m)^\s*(?:type|data|def)\s+([A-Za-z][A-Za-z0-9]*)""".r

  "the language surface" should "declare no mutable-cell primitive (purity precondition #3)" in {
    val offenders = elsFiles.flatMap { file =>
      val content = readFile(file)
      declarationPattern
        .findAllMatchIn(content)
        .map(_.group(1))
        .filter(forbiddenCellNames.contains)
        .map(name => s"${file.getName}: $name")
        .toList
    }
    offenders shouldBe empty
  }

  "the purity guard" should "actually find the language's .els sources (so it cannot pass vacuously)" in {
    elsFiles.map(_.getName) should contain("IO.els")
  }

  /** Every `.els` file under any layer module's `eliot/` source root (the language + platform layers). */
  private def elsFiles: Seq[File] = {
    val layerRoots = Seq("lang", "stdlib", "jvm", "compiler").map(m => new File(repoRoot, s"$m/eliot"))
    layerRoots.filter(_.isDirectory).flatMap(allFilesUnder).filter(_.getName.endsWith(".els"))
  }

  private def allFilesUnder(dir: File): Seq[File] = {
    val entries = Option(dir.listFiles()).map(_.toSeq).getOrElse(Seq.empty)
    entries.flatMap(f => if (f.isDirectory) allFilesUnder(f) else Seq(f))
  }

  /** Walk up from the test's working directory until the directory containing `build.mill` is found. */
  private def repoRoot: File = {
    @scala.annotation.tailrec
    def search(dir: File): File =
      if (new File(dir, "build.mill").isFile) dir
      else
        Option(dir.getParentFile) match {
          case Some(parent) => search(parent)
          case None         => fail("Could not locate repository root (no build.mill found above the working directory).")
        }
    search(new File(System.getProperty("user.dir")).getAbsoluteFile)
  }

  private def readFile(file: File): String = {
    val source = Source.fromFile(file, "UTF-8")
    try source.mkString
    finally source.close()
  }
}
