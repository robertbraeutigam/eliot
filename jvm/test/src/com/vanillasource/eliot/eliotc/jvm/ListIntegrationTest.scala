package com.vanillasource.eliot.eliotc.jvm

/** End-to-end coverage of `eliot.collection.List` — the immutable list represented as a native `java.util.List`. Its
  * `empty`/`append`/`foldLeft` operations are *generic* natives: emitted once, erased, and resolved from every
  * monomorphic call site by plain name + erased signature ([[NativeImplementation.genericNativeSignatures]],
  * [[ExpressionCodeGenerator]]). The two cases exercise distinct element types (`Int`, `String`) to show the one erased
  * method set serves every instantiation.
  */
class ListIntegrationTest extends FullIntegrationTest {

  "a list of ints" should "build with append and reduce with foldLeft" in {
    compileAndRun(
      """
        |import eliot.effect.Console
        |import eliot.collection.List
        |
        |def numbers: List[Int] = append(append(empty, 42), 8)
        |
        |def total(list: List[Int]): Int = foldLeft(list, 0, e -> acc -> add(e, acc))
        |
        |def main: IO[Unit] = printLine(show(total(numbers)))""".stripMargin
    ).asserting(_ shouldBe "50")
  }

  "a list of strings" should "build and fold at a different element type through the same erased natives" in {
    compileAndRun(
      """
        |import eliot.effect.Console
        |import eliot.collection.List
        |
        |def words: List[String] = append(append(empty, "first"), "last")
        |
        |def lastOr(list: List[String], start: String): String = foldLeft(list, start, e -> acc -> e)
        |
        |def main: IO[Unit] = printLine(lastOr(words, "none"))""".stripMargin
    ).asserting(_ shouldBe "last")
  }
}
