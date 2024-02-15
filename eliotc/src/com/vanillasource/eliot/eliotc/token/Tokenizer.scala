package com.vanillasource.eliot.eliotc.token

import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.feedback.{Logging, User}
import com.vanillasource.eliot.eliotc.source.{Position, PositionRange, SourceContent, Sourced, SourcedError}
import com.vanillasource.eliot.eliotc.{CompilationProcess, CompilerFact, CompilerProcessor}
import parsley.Parsley
import parsley.token.Lexer
import parsley.token.descriptions.numeric.NumericDesc
import parsley.token.descriptions.text.TextDesc
import parsley.token.descriptions.{LexicalDesc, NameDesc, SpaceDesc, SymbolDesc, numeric, text}
import parsley.token.predicate.Basic
import parsley.position.pos
import parsley.character
import parsley.errors.combinator.markAsToken
import parsley.errors.combinator.*

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
        tokens => debug(s"tokenized $file into $tokens") >> process.registerFact(SourceTokens(file, tokens))
      )

}
