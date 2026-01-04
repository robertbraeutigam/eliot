package com.vanillasource.eliot.eliotc.token

import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.processor.CompilationProcess
import com.vanillasource.eliot.eliotc.source.content.SourceContent
import com.vanillasource.eliot.eliotc.processor.common.TransformationProcessor
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerError

/** Tokenizes source content into basic building blocks: identifier, operator, literals. It gets rid of whitespace and
  * comments.
  */
class Tokenizer extends TransformationProcessor((key: SourceTokens.Key) => SourceContent.Key(key.file)) with Logging {
  override def generateFromFact(sourceContent: SourceContent): CompilerIO[Unit] = {
    val file           = sourceContent.file
    val sourcedContent = sourceContent.content

    TokenParser(sourcedContent).fullParser
      .parse(sourcedContent.value)(using new TokenErrorBuilder(sourcedContent))
      .fold(
        compilerError(_),
        tokens =>
          debug[CompilerIO](s"Tokenized $file into: ${tokens.map(_.show).mkString(", ")}.") >> registerFactIfClear(
            SourceTokens(file, sourcedContent.as(tokens))
          )
      )
  }
}
