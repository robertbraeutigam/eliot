package com.vanillasource.eliot.eliotc.jvm

class ExamplesIntegrationTest extends FullIntegrationTest {

  "hello world" should "print a string" in {
    compileAndRun("""def main: IO[Unit] = println("Hello World!")""")
      .asserting(_ shouldBe "Hello World!")
  }

  "ability" should "dispatch to correct implementation" in {
    compileAndRun(
      """ability Show[A] {
        |   def show(a: A): String
        |}
        |
        |data Hello(name: String)
        |
        |implement Show[Hello] {
        |   def show(a: Hello): String = "Hello World!"
        |}
        |
        |def main: IO[Unit] = println(show(Hello("World")))""".stripMargin
    ).asserting(_ shouldBe "Hello World!")
  }

  "ability constraint" should "pass ability through generic function" in {
    compileAndRun(
      """ability Show[A] {
        |   def show(a: A): String
        |}
        |
        |data Hello(name: String)
        |
        |implement Show[Hello] {
        |   def show(a: Hello): String = "Hello World!"
        |}
        |
        |def showAnything[A ~ Show](thing: A): String = show(thing)
        |
        |def main: IO[Unit] = println(showAnything(Hello("World")))""".stripMargin
    ).asserting(_ shouldBe "Hello World!")
  }

  "ability derive" should "derive implementation for generic type" in {
    compileAndRun(
      """ability Show[A] {
        |  def show(a: A): String
        |}
        |
        |implement Show[String] {
        |  def show(str: String): String = str
        |}
        |
        |data Box[A](content: A)
        |
        |implement[A ~ Show] Show[Box[A]] {
        |  def show(box: Box[A]): String = show(content(box))
        |}
        |
        |def main: IO[Unit] = println(show(Box("Hello World!")))""".stripMargin
    ).asserting(_ shouldBe "Hello World!")
  }

  "ability associated type" should "handle associated types in abilities" in {
    compileAndRun(
      """ability AssociatedType[T] {
        |   type MagicType
        |
        |   def handle(value: T, param: MagicType): String
        |}
        |
        |data Name(name: String)
        |
        |implement AssociatedType[Name] {
        |   type MagicType = String
        |
        |   def handle(value: Name, param: MagicType): String = "Hello"
        |}
        |
        |def main: IO[Unit] = println(handle(Name("Johnny"), "Ni"))""".stripMargin
    ).asserting(_ shouldBe "Hello")
  }

  "generic types" should "support type-level integer parameters" in {
    compileAndRun(
      """def hello[I: BigInteger]: String = "Hello World!"
        |
        |def main: IO[Unit] = println(hello[1])""".stripMargin
    ).asserting(_ shouldBe "Hello World!")
  }

  "pattern matching" should "match data constructors and extract values" in {
    compileAndRun(
      """data Maybe[A] = Nothing | Just(value: A)
        |
        |def describe(m: Maybe[String]): String = m match {
        |  case Nothing -> "empty"
        |  case Just(v) -> v
        |}
        |
        |def main: IO[Unit] = println(describe(Just("hello")))""".stripMargin
    ).asserting(_ shouldBe "hello")
  }

  "operators" should "evaluate infix operators with correct associativity" in {
    compileAndRun(
      """def main: IO[Unit] = println(content(Bool("Hello") | Bool("World") | Bool("!")))
        |
        |data Bool(content: String)
        |
        |infix left
        |def |(lhs: Bool, rhs: Bool): Bool = rhs""".stripMargin
    ).asserting(_ shouldBe "!")
  }

  "handle with" should "support multiple data types with pattern matching" in {
    compileAndRun(
      """data Something = Else | Other
        |
        |data Greeting = Hello | Goodbye
        |
        |def greet(g: Greeting): String = g match {
        |  case Hello -> "Hello!"
        |  case Goodbye -> "Goodbye!"
        |}
        |
        |def something(s: Something): String = s match {
        |  case Else -> "Else!"
        |  case Other -> "Other!"
        |}
        |
        |infix def or(s1: String, s2: String): String = s1
        |
        |def main: IO[Unit] = println(something(Else) or greet(Goodbye))""".stripMargin
    ).asserting(_ shouldBe "Else!")
  }

  "monomorph check" should "handle dependent type integer arithmetic" in {
    compileAndRun(
      """data Box[I: BigInteger](content: String)
        |
        |def someFunction[I: BigInteger](arg: String): Box[I.inc] = Box[3](arg)
        |
        |def main: IO[Unit] = println(content(someFunction[2]("Hello World!")))""".stripMargin
    ).asserting(_ shouldBe "Hello World!")
  }

  "function as type" should "use type-level computation for types" in {
    compileAndRun(
      """data Box[A](content: A)
        |
        |def stringBox: Type = Box[String]
        |
        |def stringBoxWithContent: stringBox = Box("Hello World!")
        |
        |def main: IO[Unit] = println(content(stringBoxWithContent))""".stripMargin
    ).asserting(_ shouldBe "Hello World!")
  }

  "type values" should "match on type-level values" in {
    compileAndRun(
      """data Person[NAME: String](content: String)
        |
        |data Box[A](a: A)
        |
        |def personName(t: Type): String = t match {
        |   case Person[name] -> name
        |   case _            -> "<not a person>"
        |}
        |
        |def main: IO[Unit] = println(personName(Person["John"]))""".stripMargin
    ).asserting(_ shouldBe "John")
  }

  "dot operator" should "support method-style chaining" in {
    compileAndRun(
      """data Box[A](content: A)
        |
        |def filter[A](expr: String, box: Box[A]): Box[A] = box
        |
        |def map[A, B](f: Function[A, B], box: Box[A]): Box[B] = Box(f(content(box)))
        |
        |def flatMap[A, B](f: Function[A, Box[B]], box: Box[A]): Box[B] = f(content(box))
        |
        |def as[A, B](b: B, box: Box[A]): Box[B] = box.map(_ -> b)
        |
        |def logic: Box[String] = Box("Hello").filter("Expr").map(_ -> "Earth!").as("World!")
        |
        |def main: IO[Unit] = println(logic.content)""".stripMargin
    ).asserting(_ shouldBe "World!")
  }

  "unicode" should "support unicode operator names" in {
    compileAndRun(
      """def main: IO[Unit] = println(<===>)
        |
        |def <===>: String = "Hello World!"""".stripMargin
    ).asserting(_ shouldBe "Hello World!")
  }

  "integer addition" should "compute and print a sum at runtime" in {
    compileAndRun(
      """def main: IO[Unit] = println(intToString(3 + 4))""".stripMargin
    ).asserting(_ shouldBe "7")
  }

  "integer subtraction" should "compute and print a difference at runtime" in {
    compileAndRun(
      """def main: IO[Unit] = println(intToString(10 - 4))""".stripMargin
    ).asserting(_ shouldBe "6")
  }

  "integer arithmetic" should "respect operator precedence at runtime" in {
    compileAndRun(
      """def main: IO[Unit] = println(intToString(2 + 3 * 4))""".stripMargin
    ).asserting(_ shouldBe "14")
  }

  it should "compute a negative result at runtime" in {
    compileAndRun(
      """def main: IO[Unit] = println(intToString(3 - 10))""".stripMargin
    ).asserting(_ shouldBe "-7")
  }

  // Byte operands whose sum overflows a byte carry up to a Short result (`nativeAddByteToShort`): 100 and 100 each fit
  // `Byte` ([-128,127]) but 200 does not, so the result is laid out at the wider representation.
  it should "carry a byte-operand sum into a wider result representation at runtime" in {
    compileAndRun(
      """def main: IO[Unit] = println(intToString(100 + 100))""".stripMargin
    ).asserting(_ shouldBe "200")
  }

  // Short operands whose difference fits a byte: 1000 and 999 are `Short`, but the result range [1,1] fits `Byte`, so
  // the surrounding `nativeWiden` narrows the leaf's result back down (additive cancellation).
  it should "narrow a short-operand difference into a byte result at runtime" in {
    compileAndRun(
      """def main: IO[Unit] = println(intToString(1000 - 999))""".stripMargin
    ).asserting(_ shouldBe "1")
  }

  "integer range widening" should "widen a bare literal into a broader declared range at runtime" in {
    compileAndRun(
      """def widened: Int[0, 1000] = 7
        |
        |def main: IO[Unit] = println(intToString(widened))""".stripMargin
    ).asserting(_ shouldBe "7")
  }

  it should "accept a literal assigned through a width alias and print it" in {
    compileAndRun(
      """def small: Byte = 42
        |
        |def main: IO[Unit] = println(intToString(small))""".stripMargin
    ).asserting(_ shouldBe "42")
  }

  it should "construct and access a record with a bare Int field (W2)" in {
    // `Counter`'s bare `Int` field generalizes the type to `Counter[lo, hi]`; the accessor `n` (a match under the hood)
    // recovers the field. Exercises construct + accessor + handleCases end-to-end at runtime.
    compileAndRun(
      """data Counter(n: Int)
        |
        |def main: IO[Unit] = println(intToString(n(Counter(42))))""".stripMargin
    ).asserting(_ shouldBe "42")
  }

  it should "round-trip a record field through an explicit match (W2)" in {
    compileAndRun(
      """data Counter(n: Int)
        |
        |def field(c: Counter[7, 7]): Int[7, 7] = c match { case Counter(x) -> x }
        |
        |def main: IO[Unit] = println(intToString(field(Counter(7))))""".stripMargin
    ).asserting(_ shouldBe "7")
  }

  // W2 follow-up: type-level matching over an auto-bounded record. `Counter`'s bare `Int` field grows the type to
  // `Counter[lo, hi]`, so the `typeMatch` matcher's handler must bind both synthesized bounds (`case Counter[lo, hi]`).
  it should "type-level match over an auto-bounded record (W2 follow-up)" in {
    compileAndRun(
      """data Counter(n: Int)
        |
        |def describe(t: Type): String = t match {
        |   case Counter[lo, hi] -> "counter"
        |   case _               -> "<other>"
        |}
        |
        |def main: IO[Unit] = println(describe(Counter[0, 255]))""".stripMargin
    ).asserting(_ shouldBe "counter")
  }

  it should "widen an arithmetic result into a broader declared range at runtime" in {
    compileAndRun(
      """def total: Int[0, 1000] = 3 + 4
        |
        |def main: IO[Unit] = println(intToString(total))""".stripMargin
    ).asserting(_ shouldBe "7")
  }

  // Phase 3: representation selection. Each range below lowers to a different JVM wrapper (Short/Integer/Long); a wrong
  // width would overflow and print the wrong number, so these assert the chosen representation is wide enough.
  "integer representation selection" should "carry an Int[0, 70000] value at the 32-bit representation" in {
    compileAndRun(
      """def big: Int[0, 70000] = 70000
        |
        |def main: IO[Unit] = println(intToString(big))""".stripMargin
    ).asserting(_ shouldBe "70000")
  }

  it should "compute a product that overflows 16 bits at the 32-bit representation" in {
    compileAndRun(
      """def product: Int[0, 1000000] = 1000 * 1000
        |
        |def main: IO[Unit] = println(intToString(product))""".stripMargin
    ).asserting(_ shouldBe "1000000")
  }

  it should "carry a value beyond 32 bits at the 64-bit representation" in {
    compileAndRun(
      """def huge: Int[0, 5000000000] = 5000000000
        |
        |def main: IO[Unit] = println(intToString(huge))""".stripMargin
    ).asserting(_ shouldBe "5000000000")
  }

  // Mangling/dedup: a generic function instantiated at two ranges that share a representation (both `Byte`) lowers to a
  // single concrete signature, so the per-range mangled methods collapse correctly and both calls dispatch.
  "generic instantiation" should "reuse one method for two same-representation ranges at runtime" in {
    compileAndRun(
      """def id[Mn: BigInteger, Mx: BigInteger](x: Int[Mn, Mx]): Int[Mn, Mx] = x
        |def a: Int[0, 3] = 3
        |def b: Int[0, 5] = 5
        |
        |def main: IO[Unit] = println(intToString(id(a) + id(b)))""".stripMargin
    ).asserting(_ shouldBe "8")
  }

  // The same generic at two ranges with *different* representations (`Byte` and `Long`) lowers to two distinct concrete
  // signatures — collision-free overloads — and each call dispatches to the one matching its width.
  it should "dispatch distinct methods for two different-representation ranges at runtime" in {
    compileAndRun(
      """def id[Mn: BigInteger, Mx: BigInteger](x: Int[Mn, Mx]): Int[Mn, Mx] = x
        |def a: Int[0, 3] = 3
        |def big: Int[0, 5000000000] = 5000000000
        |
        |def main: IO[Unit] = println(intToString(id(a) + id(big)))""".stripMargin
    ).asserting(_ shouldBe "5000000003")
  }

  // Eliminating a multi-field `data` desugars its `match` into an N-parameter handler (`x -> y -> body`), which the
  // backend lowers into nested single-argument closures. Extracting the *first* field exercises the outer closure.
  "multi-field data" should "extract the first field of a two-field constructor" in {
    compileAndRun(
      """data Pair(a: String, b: String)
        |
        |def first(p: Pair): String = p match {
        |  case Pair(x, y) -> x
        |}
        |
        |def main: IO[Unit] = println(first(Pair("hello", "world")))""".stripMargin
    ).asserting(_ shouldBe "hello")
  }

  // Extracting the *second* field returns the inner closure's parameter, so it checks the captured-variable plumbing
  // across the peeled frames.
  it should "extract the second field of a two-field constructor" in {
    compileAndRun(
      """data Pair(a: String, b: String)
        |
        |def second(p: Pair): String = p match {
        |  case Pair(x, y) -> y
        |}
        |
        |def main: IO[Unit] = println(second(Pair("hello", "world")))""".stripMargin
    ).asserting(_ shouldBe "world")
  }

  // A three-field constructor peels into three nested closures; selecting the middle field confirms more than two
  // levels of nesting resolve their parameters correctly.
  it should "extract the middle field of a three-field constructor" in {
    compileAndRun(
      """data Triple(a: String, b: String, c: String)
        |
        |def middle(t: Triple): String = t match {
        |  case Triple(x, y, z) -> y
        |}
        |
        |def main: IO[Unit] = println(middle(Triple("one", "two", "three")))""".stripMargin
    ).asserting(_ shouldBe "two")
  }

  // A two-field `Int` round-trip: the fields lower to different representations (`Byte`-range and `Long`-range), so
  // matching them out and summing also exercises cross-representation arithmetic over the peeled closures.
  it should "match out two integer fields at different representations and sum them" in {
    compileAndRun(
      """data IntPair(small: Int[0, 255], large: Int[0, 5000000000])
        |
        |def sum(p: IntPair): Int[0, 5000000255] = p match {
        |  case IntPair(s, l) -> s + l
        |}
        |
        |def main: IO[Unit] = println(intToString(sum(IntPair(200, 5000000000))))""".stripMargin
    ).asserting(_ shouldBe "5000000200")
  }
}
