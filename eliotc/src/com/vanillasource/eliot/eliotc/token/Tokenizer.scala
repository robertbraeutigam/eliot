package com.vanillasource.eliot.eliotc.token

import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.feedback.{Logging, User}
import com.vanillasource.eliot.eliotc.source.{SourceContent, SourcedError}
import com.vanillasource.eliot.eliotc.{CompilationProcess, CompilerFact, CompilerFactKey, CompilerProcessor}
import parsley.token.descriptions.{numeric, text}

import java.nio.file.Path

/** Tokenizes source content into basic building blocks: identifier, operator, literals. It gets rid of whitespace and
  * comments.
  */
class Tokenizer extends CompilerProcessor with Logging with User {
  override def generate(factKey: CompilerFactKey)(using process: CompilationProcess): IO[Unit] = factKey match {
    case SourceTokens.Key(path) =>
      process.getFact(SourceContent.Key(path)).map(_.traverse_(processFact))
    case _                      => IO.unit
  }
  override def processFact(fact: CompilerFact)(using CompilationProcess): IO[Unit]             = fact match {
    case SourceContent(path, rootPath, content) => tokenize(path, rootPath, content)
    case _                                      => IO.unit
  }

  private def tokenize(path: Path, rootPath: Path, content: String)(using process: CompilationProcess): IO[Unit] = {
    val file = rootPath.resolve(path).toFile

    TokenParser(file).fullParser
      .parse(content)(using new TokenErrorBuilder(file))
      .fold(
        errorMessage => SourcedError.registerCompilerError(errorMessage),
        tokens =>
          debug(s"tokenized $file into: ${tokens.map(_.show).mkString(", ")}") >> process.registerFact(
            SourceTokens(path, rootPath, tokens)
          )
      )
  }

}
