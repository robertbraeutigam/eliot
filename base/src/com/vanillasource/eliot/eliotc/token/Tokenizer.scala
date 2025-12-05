package com.vanillasource.eliot.eliotc.token

import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.CompilationProcess
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.processor.OneToOneProcessor
import com.vanillasource.eliot.eliotc.source.content.SourceContent
import com.vanillasource.eliot.eliotc.source.error.SourcedError
import com.vanillasource.eliot.eliotc.source.pos.Sourced

import java.nio.file.Path

/** Tokenizes source content into basic building blocks: identifier, operator, literals. It gets rid of whitespace and
  * comments.
  */
class Tokenizer extends OneToOneProcessor((key: SourceTokens.Key) => SourceContent.Key(key.file)) with Logging {
  override def generateFromFact(sourceContent: SourceContent)(using process: CompilationProcess): IO[Unit] = {
    val file           = sourceContent.file
    val sourcedContent = sourceContent.content

    TokenParser(sourcedContent).fullParser
      .parse(sourcedContent.value)(using new TokenErrorBuilder(sourcedContent))
      .fold(
        errorMessage => SourcedError.registerCompilerError(errorMessage),
        tokens =>
          debug(s"Tokenized $file into: ${tokens.map(_.show).mkString(", ")}.") >>
            process.registerFact(SourceTokens(file, sourcedContent.as(tokens)))
      )
  }
}
