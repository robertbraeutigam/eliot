package com.vanillasource.eliot.eliotc.lsp.index

import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, QualifiedName, Qualifier}
import com.vanillasource.eliot.eliotc.pos.PositionRange
import com.vanillasource.eliot.eliotc.resolve.fact.ResolvedValue

import java.net.URI

/** Index of test suites: maps a document to its `testCases`, if it declares one.
  *
  * A module holds a suite exactly when it declares a value named `testCases` in the default namespace — the name
  * `eliot.test`'s runner gathers by (`foldNamedValues("testCases", …)`), so "is a test" is the runner's own rule and the
  * editor guesses nothing. Like [[MainIndex]] it is rebuilt from the workspace's [[ResolvedValue]] facts after every
  * compile, and carries the `testCases` name's source range (where the lens is anchored) and the declaring
  * [[ModuleName]] — which is also what selects the suite when the runner is started (`Runner <module>`).
  *
  * Whether a package *can* run its suites is not decided here: that is whether the runner is on the package's path
  * ([[TestIndex.runnerModule]]), which the compilation service checks per session.
  */
final class TestIndex private (suitesByUri: Map[String, TestIndex.Entry]) {

  /** The suite declared in the given document, or `None` if it declares none. */
  def testsAt(uri: URI): Option[TestIndex.Entry] = suitesByUri.get(MainIndex.uriKey(uri))
}

object TestIndex {

  /** A test suite: the source range of its `testCases` name (the lens anchor) and the module that declares it. */
  case class Entry(range: PositionRange, moduleName: ModuleName)

  val empty: TestIndex = new TestIndex(Map.empty)

  /** The module whose `main` runs the suites of a program — what a package's `suite` dependency puts on its path. */
  val runnerModule: ModuleName = ModuleName(Seq("eliot", "test"), "Runner")

  private val suiteName = QualifiedName("testCases", Qualifier.Default)

  /** Build the index from all resolved values in the workspace, keeping only those named `testCases`. A document
    * declares at most one, so the last wins on the (degenerate) duplicate case — which the compiler would already have
    * flagged as a redefinition.
    */
  def build(values: Seq[ResolvedValue]): TestIndex =
    new TestIndex(
      values
        .filter(_.vfqn.name == suiteName)
        .map(value => MainIndex.uriKey(value.name.uri) -> Entry(value.name.range, value.vfqn.moduleName))
        .toMap
    )
}
