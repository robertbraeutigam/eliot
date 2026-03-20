package com.vanillasource.eliot.eliotc.jvm

class EliotIntegrationSpec extends EliotIntegrationTest {

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

  "ability associated type" should "handle associated types in abilities" ignore {
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

  "generic types" should "support type-level integer parameters" ignore {
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

  "handle with" should "support multiple data types with pattern matching" ignore {
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

  "monomorph check" should "handle dependent type integer arithmetic" ignore {
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

  "type values" should "match on type-level values" ignore {
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
}
