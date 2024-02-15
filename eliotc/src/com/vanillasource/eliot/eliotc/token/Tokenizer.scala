package com.vanillasource.eliot.eliotc.token

import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.feedback.{Logging, User}
import com.vanillasource.eliot.eliotc.source.{SourceContent, SourcedError}
import com.vanillasource.eliot.eliotc.{CompilationProcess, CompilerFact, CompilerProcessor}
import parsley.token.descriptions.{numeric, text}

import java.io.File

/** Tokenizes source content into basic building blocks: identifier, operator, literals. It gets rid of whitespace and
  * comments.
  */
class Tokenizer extends CompilerProcessor with Logging with User {
  override def process(fact: CompilerFact)(using CompilationProcess): IO[Unit] = fact match {
    case SourceContent(file, content) => tokenize(file, content)
    case _                            => IO.unit
  }

  private def tokenize(file: File, content: String)(using process: CompilationProcess): IO[Unit] =
    TokenParser(file).fullParser
      .parse(content)(using new TokenErrorBuilder(file))
      .fold(
        errorMessage => SourcedError.compilerError(errorMessage),
        tokens =>
          debug(s"tokenized $file into: ${tokens.map(_.show).mkString(", ")}") >> process.registerFact(
            SourceTokens(file, tokens)
          )
      )

}
