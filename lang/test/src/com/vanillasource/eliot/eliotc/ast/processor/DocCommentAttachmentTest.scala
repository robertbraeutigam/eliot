package com.vanillasource.eliot.eliotc.ast.processor

import cats.effect.IO
import com.vanillasource.eliot.eliotc.ProcessorTest
import com.vanillasource.eliot.eliotc.ast.fact.{AST, SourceAST}
import com.vanillasource.eliot.eliotc.token.Tokenizer

class DocCommentAttachmentTest extends ProcessorTest(new Tokenizer(), new ASTParser()) {
  "doc comment attachment" should "attach a doc comment to the function it precedes" in {
    functionDoc("/** Doc for foo. */\ndef foo: Unit", "foo").asserting(_ shouldBe Some(" Doc for foo. "))
  }

  it should "attach a doc comment to the data definition it precedes" in {
    dataDoc("/** The colour type. */\ndata Color", "Color").asserting(_ shouldBe Some(" The colour type. "))
  }

  it should "leave a function with no preceding doc comment undocumented" in {
    functionDoc("/** Doc for foo. */\ndef foo: Unit\n\ndef bar: Unit", "bar").asserting(_ shouldBe None)
  }

  it should "bind a doc comment only to its nearest following declaration" in {
    functionDoc("/** Doc for foo. */\ndef foo: Unit", "foo").asserting(_ shouldBe Some(" Doc for foo. "))
  }

  it should "drop a trailing doc comment with no following declaration" in {
    functionDoc("def x: Unit\n/** orphan */", "x").asserting(_ shouldBe None)
  }

  it should "pick the closest doc comment when several precede one declaration" in {
    functionDoc("/** far */\n/** near */\ndef y: Unit", "y").asserting(_ shouldBe Some(" near "))
  }

  private def parseAst(source: String): IO[AST] =
    runGenerator(source, SourceAST.Key(file))
      .map(_._2.get(SourceAST.Key(file)).map(_.asInstanceOf[SourceAST].ast.value).get)

  private def functionDoc(source: String, name: String): IO[Option[String]] =
    parseAst(source).map(_.functionDefinitions.find(_.name.value.name == name).flatMap(_.doc).map(_.value))

  private def dataDoc(source: String, name: String): IO[Option[String]] =
    parseAst(source).map(_.typeDefinitions.find(_.name.value == name).flatMap(_.doc).map(_.value))
}
