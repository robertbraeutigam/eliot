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
class Tokenizer
    extends TransformationProcessor[SourceContent.Key, SourceTokens.Key]
    with Logging {

  override protected def getInputKey(outputKey: SourceTokens.Key): SourceContent.Key =
    SourceContent.Key(outputKey.file)

  override protected def generateFromKeyAndFact(key: SourceTokens.Key, sourceContent: SourceContent): CompilerIO[SourceTokens] = {
    val file           = sourceContent.file
    val sourcedContent = sourceContent.content

    TokenParser(sourcedContent).fullParser
      .parse(sourcedContent.value)(using new TokenErrorBuilder(sourcedContent))
      .fold(
        err => compilerError(err) >> abort[SourceTokens],
        tokens =>
          debug[CompilerIO](s"Tokenized $file into: ${tokens.map(_.show).mkString(", ")}.").as(
            SourceTokens(file, sourcedContent.as(tokens))
          )
      )
  }
}
