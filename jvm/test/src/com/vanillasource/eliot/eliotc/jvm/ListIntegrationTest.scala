package com.vanillasource.eliot.eliotc.jvm

/** End-to-end coverage of `eliot.collection.List` — the immutable list represented as a native `java.util.List`. Its
  * `empty`/`append`/`foldLeft` operations are *generic* natives: emitted once, erased, and resolved from every
  * monomorphic call site by plain name + erased signature ([[NativeImplementation.genericNativeSignatures]],
  * [[ExpressionCodeGenerator]]). The cases exercise distinct element types (`Int`, `String`) to show the one erased
  * method set serves every instantiation, plus ordering, the empty list, and append's value semantics.
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
        |def total(list: List[Int]): Int = list.foldLeft(0, e -> acc -> add(e, acc))
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
        |def lastOr(list: List[String], start: String): String = list.foldLeft(start, e -> acc -> e)
        |
        |def main: IO[Unit] = printLine(lastOr(words, "none"))""".stripMargin
    ).asserting(_ shouldBe "last")
  }

  "foldLeft" should "visit elements front to back (append order)" in {
    // A non-commutative combine `acc*10 + e` turns the list into a decimal number, so the result reveals the visiting
    // order: append(append(append(empty, 1), 2), 3) folds to 123, not 321.
    compileAndRun(
      """
        |import eliot.effect.Console
        |import eliot.collection.List
        |
        |def digits: List[Int] = append(append(append(empty, 1), 2), 3)
        |
        |def toNumber(list: List[Int]): Int = list.foldLeft(0, e -> acc -> add(multiply(acc, 10), e))
        |
        |def main: IO[Unit] = printLine(show(toNumber(digits)))""".stripMargin
    ).asserting(_ shouldBe "123")
  }

  "foldLeft over the empty list" should "return the initial value untouched" in {
    compileAndRun(
      """
        |import eliot.effect.Console
        |import eliot.collection.List
        |
        |def emptyInts: List[Int] = empty
        |
        |def sumFrom(list: List[Int], start: Int): Int = list.foldLeft(start, e -> acc -> add(e, acc))
        |
        |def main: IO[Unit] = printLine(show(sumFrom(emptyInts, 7)))""".stripMargin
    ).asserting(_ shouldBe "7")
  }

  "a list" should "report its length via a counting foldLeft" in {
    compileAndRun(
      """
        |import eliot.effect.Console
        |import eliot.collection.List
        |
        |def items: List[String] = append(append(append(empty, "a"), "b"), "c")
        |
        |def size(list: List[String]): Int = list.foldLeft(0, e -> acc -> add(acc, 1))
        |
        |def main: IO[Unit] = printLine(show(size(items)))""".stripMargin
    ).asserting(_ shouldBe "3")
  }

  "append" should "not mutate its source list (value semantics)" in {
    // `two` and `three` both extend `base`. If `append` mutated its source instead of copying, the shared `base` would
    // accumulate both elements and the sums would differ; because it copies, sum(two)=1+2=3 and sum(three)=1+3=4, so 7.
    compileAndRun(
      """
        |import eliot.effect.Console
        |import eliot.collection.List
        |
        |def base: List[Int] = append(empty, 1)
        |def two: List[Int] = append(base, 2)
        |def three: List[Int] = append(base, 3)
        |
        |def sum(list: List[Int]): Int = list.foldLeft(0, e -> acc -> add(e, acc))
        |
        |def main: IO[Unit] = printLine(show(add(sum(two), sum(three))))""".stripMargin
    ).asserting(_ shouldBe "7")
  }
}
