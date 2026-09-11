package com.vanillasource.eliot.eliotc.core.processor

import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.ProcessorTest
import com.vanillasource.eliot.eliotc.ast.processor.ASTParser
import com.vanillasource.eliot.eliotc.core.fact.CoreAST
import com.vanillasource.eliot.eliotc.core.fact.NamedValue.render
import com.vanillasource.eliot.eliotc.token.Tokenizer

/** A **row alias** used as a definition's return type is spliced in before anything else runs, so the definition is
  * the one the user could have written by hand.
  *
  * The identity assertions are the whole point: each compares the lowered signature of a definition that names the
  * alias against the same definition with the row written out. Equality there is what says the feature is a syntactic
  * expansion and not a second way for a row to reach the compiler — nothing downstream can tell them apart, which is
  * why the binder minting, the `row` phase's scope check and codegen all need no knowledge of it.
  */
class RowAliasExpanderTest extends ProcessorTest(Tokenizer(), ASTParser(), CoreProcessor()) {
  private val positionViolation =
    "Row alias 'Talk' can only name a definition's return type. " +
      "A row on a parameter is supplied rather than received, so it must be written out."

  "a row alias naming a return type" should "lower exactly as the written-out row does" in {
    identical("type Talk[A] = {Console} A\ndef greet(name: String): Talk[Unit] = x", "def greet(name: String): {Console} Unit = x")
      .asserting(_ shouldBe true)
  }

  "a parameterless row alias" should "lower exactly as the written-out row does" in {
    identical("type Talk = {Console} Unit\ndef greet: Talk = x", "def greet: {Console} Unit = x")
      .asserting(_ shouldBe true)
  }

  "a multi-entry row alias" should "lower exactly as the written-out row does" in {
    identical(
      "type Git[A] = {Process, FileSystem, Throw[IoError], Throw[GitError]} A\ndef tags(root: Path): Git[List[TagRef]] = x",
      "def tags(root: Path): {Process, FileSystem, Throw[IoError], Throw[GitError]} List[TagRef] = x"
    ).asserting(_ shouldBe true)
  }

  "a row alias argument" should "be substituted into the row's own entries, not only its payload" in {
    identical(
      "type Fallible[E, A] = {Throw[E]} A\ndef risky: Fallible[IoError, String] = x",
      "def risky: {Throw[IoError]} String = x"
    ).asserting(_ shouldBe true)
  }

  "an ordinary alias over a row alias's payload" should "be left alone" in {
    identical("type Talk[A] = {Console} A\ndef greet: Unit = x", "def greet: Unit = x").asserting(_ shouldBe true)
  }

  "a row alias on a parameter" should "be rejected, since a parameter row is supplied and thunked" in {
    errors("type Talk[A] = {Console} A\ndef run(body: Talk[Unit]): Unit = x")
      .asserting(_ shouldBe Seq(positionViolation at "Talk"))
  }

  "a row alias nested inside a return type" should "be rejected" in {
    errors("type Talk[A] = {Console} A\ndef held: Option[Talk[Unit]] = x")
      .asserting(_ shouldBe Seq(positionViolation at "Talk"))
  }

  "a row alias applied to the wrong number of arguments" should "be rejected" in {
    errors("type Talk[A] = {Console} A\ndef greet: Talk = x")
      .asserting(_ shouldBe Seq("Row alias 'Talk' takes 1 type argument(s), but 0 were given." at "Talk"))
  }

  "a type alias whose body is not a row" should "name a return type like any other" in {
    errors("type Name = String\ndef greet: Name = x").asserting(_ shouldBe empty)
  }

  /** Whether the definition named `greet`/`tags`/`risky`/`held` lowers identically in both sources. */
  private def identical(withAlias: String, writtenOut: String): IO[Boolean] =
    (signatures(withAlias), signatures(writtenOut)).mapN { (aliased, plain) =>
      plain.nonEmpty && plain.forall(signature => aliased.contains(signature))
    }

  /** Every lowered named value of a source, rendered — the alias itself is dropped from the comparison by name. */
  private def signatures(source: String): IO[Set[String]] =
    runGenerator(source, CoreAST.Key(file)).map { case (_, facts) =>
      facts.values
        .collectFirst { case ast: CoreAST => ast }
        .toSeq
        .flatMap(_.ast.value.namedValues)
        .filterNot(_.qualifiedName.value.name === "Talk")
        .filterNot(_.qualifiedName.value.name === "Git")
        .filterNot(_.qualifiedName.value.name === "Fallible")
        .map(_.render)
        .toSet
    }

  private def errors(source: String): IO[Seq[TestError]] =
    runGenerator(source, CoreAST.Key(file)).map(result => toTestErrors(result._1))
}
