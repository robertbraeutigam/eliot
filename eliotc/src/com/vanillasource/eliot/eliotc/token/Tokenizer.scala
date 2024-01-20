package com.vanillasource.eliot.eliotc.token

import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.feedback.{Logging, User}
import com.vanillasource.eliot.eliotc.source.SourceContent
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
import parsley.errors.combinator._

import java.io.File

/** Tokenizes source content into basic building blocks: identifier, operator, literals. It gets rid of whitespace and
  * comments.
  */
class Tokenizer extends CompilerProcessor with Logging with User {
  private val lexer = new Lexer(
    LexicalDesc(
      NameDesc(
        identifierStart = Basic(_.isLetter),
        identifierLetter = Basic(c => c.isLetterOrDigit),
        operatorStart = Basic(":!#$%&*+./<=>?@\\^|-~;".contains(_)),
        operatorLetter = Basic(":!#$%&*+./<=>?@\\^|-~;".contains(_))
      ),
      SymbolDesc(
        hardKeywords = Set("import", "native"),
        hardOperators = Set("(", ")"),
        caseSensitive = true
      ),
      NumericDesc.plain,
      TextDesc.plain,
      SpaceDesc(
        commentStart = "/*",
        commentEnd = "*/",
        commentLine = "//",
        commentLineAllowsEOF = true,
        nestedComments = false,
        space = Basic(_.isWhitespace),
        whitespaceIsContextDependent = false
      )
    )
  )

  private lazy val fullParser: Parsley[List[Sourced[Token]]] = lexer.fully(Parsley.many(tokenParser))

  private lazy val tokenParser: Parsley[Sourced[Token]] =
    identifier <|> symbolParser <|> standaloneSymbolParser <|> keywords

  private lazy val symbolParser: Parsley[Sourced[Token.Symbol]] = sourcedLexeme(
    lexer.nonlexeme.names.userDefinedOperator.map(Token.Symbol.apply)
  )

  private lazy val standaloneSymbolParser: Parsley[Sourced[Token.Symbol]] = sourcedLexeme(
    character.oneOf('(', ')', ',').map(_.toString).map(Token.Symbol.apply)
  ).label("special operator")

  private lazy val keywords: Parsley[Sourced[Token.Keyword]] = sourcedLexeme(
    character.strings("import", "native").map(Token.Keyword.apply)
  ).label("keyword")

  private lazy val identifier: Parsley[Sourced[Token.Identifier]] = sourcedLexeme(
    lexer.nonlexeme.names.identifier.map(Token.Identifier.apply)
  )

  private lazy val position: Parsley[Position] = pos.map(Position.apply.tupled)

  override def process(fact: CompilerFact[_])(using CompilationProcess): IO[Unit] = fact match {
    case SourceContent(file, content) => tokenize(file, content)
    case _                            => IO.unit
  }

  private def tokenize(file: File, content: String)(using process: CompilationProcess): IO[Unit] =
    fullParser
      .parse(content)(using new TokenErrorBuilder())
      .fold(
        errorMessage =>
          compilerError(
            file,
            content,
            errorMessage.range.from.line,
            errorMessage.range.from.col,
            errorMessage.range.to.line,
            errorMessage.range.to.col,
            errorMessage.value
          ),
        tokens => debug(s"tokenized $file into $tokens") >> process.registerFact(SourceTokens(file, tokens))
      )

  private def sourcedLexeme[T](parser: Parsley[T]): Parsley[Sourced[T]] = for {
    from <- position
    t    <- markAsToken(parser)
    to   <- position
    _    <- lexer.space.whiteSpace
  } yield Sourced(PositionRange(from, to), t)
}
