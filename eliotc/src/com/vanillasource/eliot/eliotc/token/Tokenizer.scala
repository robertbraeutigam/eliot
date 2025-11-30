package com.vanillasource.eliot.eliotc.token

import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.source.error.SourcedError
import com.vanillasource.eliot.eliotc.source.pos.Sourced
import com.vanillasource.eliot.eliotc.source.resolve.ResolvedSourceContent
import com.vanillasource.eliot.eliotc.{CompilationProcess, CompilerFact, CompilerFactKey, CompilerProcessor}

import java.nio.file.Path

/** Tokenizes source content into basic building blocks: identifier, operator, literals. It gets rid of whitespace and
  * comments.
  */
class Tokenizer extends CompilerProcessor with Logging {
  override def generate(factKey: CompilerFactKey[_])(using process: CompilationProcess): IO[Unit] = factKey match {
    case SourceTokens.Key(path) =>
      process.getFact(ResolvedSourceContent.Key(path)).flatMap(_.traverse_(processFact))
    case _                      => IO.unit
  }
  private def processFact(fact: CompilerFact)(using CompilationProcess): IO[Unit]                 = fact match {
    case ResolvedSourceContent(path, sourcedContent) => tokenize(path, sourcedContent)
    case _                                           => IO.unit
  }

  private def tokenize(path: Path, sourcedContent: Sourced[String])(using process: CompilationProcess): IO[Unit] =
    TokenParser(sourcedContent).fullParser
      .parse(sourcedContent.value)(using new TokenErrorBuilder(sourcedContent))
      .fold(
        errorMessage => SourcedError.registerCompilerError(errorMessage),
        tokens =>
          debug(s"Tokenized $path into: ${tokens.map(_.show).mkString(", ")}.") >>
            process.registerFact(SourceTokens(path, sourcedContent.as(tokens)))
      )

}
