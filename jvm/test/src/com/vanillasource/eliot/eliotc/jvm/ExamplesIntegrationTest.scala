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
}
