package com.vanillasource.eliot.eliotc.core.processor

import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.ProcessorTest
import com.vanillasource.eliot.eliotc.ast.processor.ASTParser
import com.vanillasource.eliot.eliotc.core.fact.Expression.*
import com.vanillasource.eliot.eliotc.core.fact.{CoreAST, Expression, NamedValue}
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.token.Tokenizer

/** A **row alias** (`type Git[A] = {Process, FileSystem} A`) names a row together with its payload, and a definition
  * naming one as its return type receives that row.
  *
  * The alias is an *ordinary type alias* and the use an *ordinary application*: what crosses the use site is the row's
  * **entries**, never its payload. So two things are asserted separately, and they are the whole feature — that the
  * definition mints and declares exactly what the written-out row mints and declares (nothing downstream can tell the
  * two apart, which is why the binder minting, the scope check and codegen need no knowledge of aliases), and that its
  * *return type* still says `Talk[Unit]`, the name the user wrote, for the evaluator to reduce.
  */
class RowAliasesTest extends ProcessorTest(Tokenizer(), ASTParser(), CoreProcessor()) {
  private val positionViolation =
    "Row alias 'Talk' can only name a definition's return type. " +
      "A row on a parameter is supplied rather than received, so it must be written out."

  "a def naming a row alias" should "mint and declare exactly what the written-out row does" in {
    (
      rowLowering("type Talk[A] = {Console} A\ndef greet(name: String): Talk[Unit] = x", "greet"),
      rowLowering("def greet(name: String): {Console} Unit = x", "greet")
    ).mapN(_ === _).asserting(_ shouldBe true)
  }

  "a def naming a parameterless row alias" should "mint and declare exactly what the written-out row does" in {
    (
      rowLowering("type Talk = {Console} Unit\ndef greet: Talk = x", "greet"),
      rowLowering("def greet: {Console} Unit = x", "greet")
    ).mapN(_ === _).asserting(_ shouldBe true)
  }

  "a def naming a multi-entry row alias" should "mint and declare exactly what the written-out row does" in {
    (
      rowLowering(
        "type Git[A] = {Process, FileSystem, Throw[IoError]} A\ndef tags(root: Path): Git[List[TagRef]] = x",
        "tags"
      ),
      rowLowering("def tags(root: Path): {Process, FileSystem, Throw[IoError]} List[TagRef] = x", "tags")
    ).mapN(_ === _).asserting(_ shouldBe true)
  }

  "a row alias argument" should "be substituted into the row's own entries" in {
    rowLowering("type Fallible[E, A] = {Throw[E]} A\ndef risky: Fallible[IoError, String] = x", "risky")
      .asserting(_ shouldBe (Seq("Throw"), Seq("Throw[IoError]")))
  }

  "the return type naming a row alias" should "stay the application the user wrote" in {
    signature("type Talk[A] = {Console} A\ndef greet(name: String): Talk[Unit] = x", "greet")
      .asserting(_ shouldBe "Impl -> Function(String)(Talk(Unit))")
  }

  "a row alias itself" should "lower to an ordinary type alias, with no binding binder" in {
    signature("type Talk[A] = {Console} A\ndef greet: Talk[Unit] = x", "Talk")
      .asserting(_ shouldBe "Function(Type)(Type)")
  }

  it should "lower to its payload, the row erased" in {
    body("type Talk[A] = {Console} A\ndef greet: Talk[Unit] = x", "Talk").asserting(_ shouldBe Some("A -> A"))
  }

  "a row alias on a parameter" should "be rejected, since a parameter row is supplied and thunked" in {
    errors("type Talk[A] = {Console} A\ndef run(body: Talk[Unit]): Unit = x")
      .asserting(_ shouldBe Seq(positionViolation at "Talk"))
  }

  "a row alias nested inside a return type" should "be rejected" in {
    errors("type Talk[A] = {Console} A\ndef held: Option[Talk[Unit]] = x")
      .asserting(_ shouldBe Seq(positionViolation at "Talk"))
  }

  "a row alias named by another type alias" should "be rejected, since no type receives a row" in {
    errors("type Talk[A] = {Console} A\ntype Held[A] = Option[Talk[A]]\ndef held: Held[Unit] = x")
      .asserting(_ shouldBe Seq(positionViolation at "Talk"))
  }

  "a row alias applied to the wrong number of arguments" should "be rejected" in {
    errors("type Talk[A] = {Console} A\ndef greet: Talk = x")
      .asserting(_ shouldBe Seq("Row alias 'Talk' takes 1 type argument(s), but 0 were given." at "Talk"))
  }

  "a type alias whose body is not a row" should "name a return type like any other" in {
    errors("type Name = String\ndef greet: Name = x").asserting(_ shouldBe empty)
  }

  /** A definition's **row-relevant** lowering: the abilities its binding binders mark, and its declared return row.
    * Together they are everything a received row leaves behind — what the marks say, and what the row records.
    */
  private def rowLowering(source: String, name: String): IO[(Seq[String], Seq[String])] =
    value(source, name).map(v => (bindings(v), declaredRow(v)))

  /** The abilities this definition's binding binders mark — its leading binders that carry the implementation mark. */
  private def bindings(value: NamedValue): Seq[String] =
    binders(value.signature.value).flatMap(_._2).collect {
      case FunctionApplication(Sourced(_, _, NamedValueReference(marker, _, _)), Sourced(_, _, ability))
          if marker.value.name === "Implementation" =>
        simplified(ability.render)
    }

  /** The definition's declared return row, each entry as `Ability` or `Ability[args]`. */
  private def declaredRow(value: NamedValue): Seq[String] =
    value.effectRow.returnEffects.map { entry =>
      val arguments = entry.typeArgs.map(argument => simplified(argument.render))
      entry.abilityName.value + (if (arguments.isEmpty) "" else arguments.mkString("[", ",", "]"))
    }

  /** The definition's signature, rendered with the qualified-name noise stripped. */
  private def signature(source: String, name: String): IO[String] =
    value(source, name).map(v => simplified(v.signature.value.render))

  private def body(source: String, name: String): IO[Option[String]] =
    value(source, name).map(_.runtime.map(r => simplified(r.value.render)))

  private def binders(expression: Expression): Seq[(String, Option[Expression])] = expression match {
    case FunctionLiteral(name, parameterType, body) => (name.value, parameterType.map(_.value)) +: binders(body.value)
    case _                                          => Seq.empty
  }

  private def simplified(rendered: String): String = rendered.replaceAll("QualifiedName\\((\\w+),[^)]*\\)", "$1")

  private def value(source: String, name: String): IO[NamedValue] =
    runGenerator(source, CoreAST.Key(file)).map { case (_, facts) =>
      facts.values
        .collectFirst { case ast: CoreAST => ast }
        .toSeq
        .flatMap(_.ast.value.namedValues)
        .find(_.qualifiedName.value.name === name)
        .get
    }

  private def errors(source: String): IO[Seq[TestError]] =
    runGenerator(source, CoreAST.Key(file)).map(result => toTestErrors(result._1))
}
