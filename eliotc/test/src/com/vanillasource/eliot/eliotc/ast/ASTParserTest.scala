package com.vanillasource.eliot.eliotc.ast

import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import com.vanillasource.eliot.eliotc.CompilerFact
import com.vanillasource.eliot.eliotc.main.CompilerEngine
import com.vanillasource.eliot.eliotc.source.{SourceContent, Sourced, SourcedError}
import com.vanillasource.eliot.eliotc.token.Tokenizer
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

import java.io.File

class ASTParserTest extends AsyncFlatSpec with AsyncIOSpec with Matchers {
  "ast parser" should "successfully parse empty file" in {
    parseForErrors("").asserting(_ shouldBe Seq.empty)
  }

  it should "parse a correct import statement" in {
    parseForImports("import a.b.C").asserting(_ shouldBe Seq("a.b.C"))
  }

  it should "parse multiple current import statements" in {
    parseForImports("import a.b.C\nimport b.D\nimport g.g.H").asserting(_ shouldBe Seq("a.b.C", "b.D", "g.g.H"))
  }

  private val file = new File("test.els")

  private def parseForErrors(source: String): IO[Seq[String]] =
    runTokenizer(source)
      .map(_.values.collect { case SourcedError(_, Sourced(_, msg)) => msg }.toSeq)

  private def parseForImports(source: String): IO[Seq[String]] = for {
    results <- runTokenizer(source)
  } yield {
    results.values
      .collect { case SourceAST(_, AST(statements, _)) =>
        statements.map(i => (i.packageNames.map(_.value.content) :+ i.moduleName.value.content).mkString("."))
      }
      .toSeq
      .flatten
  }

  private def runTokenizer(source: String): IO[Map[Any, CompilerFact]] =
    CompilerEngine(Seq(new Tokenizer(), new ASTParser()))
      .resolve(Seq(SourceContent(file, source)))

}
