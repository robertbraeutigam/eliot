package com.vanillasource.eliot.eliotc.token

import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.feedback.{Logging, User}
import com.vanillasource.eliot.eliotc.source.*
import com.vanillasource.eliot.eliotc.{CompilationProcess, CompilerFact, CompilerProcessor}
import parsley.{Parsley, character}
import parsley.errors.combinator.*
import parsley.position.pos
import parsley.token.Lexer
import parsley.token.descriptions.numeric.NumericDesc
import parsley.token.descriptions.text.TextDesc
import parsley.token.descriptions.*
import parsley.token.predicate.Basic

import java.io.File

class TokenParser(file: File) {
  private val lexer = new Lexer(
    LexicalDesc(
      NameDesc(
        identifierStart = Basic(_.isLetter),
        identifierLetter = Basic(c => c.isLetterOrDigit),
        operatorStart = Basic(":!#$%&*+./<=>?@\\^|-~;".contains(_)),
        operatorLetter = Basic(":!#$%&*+./<=>?@\\^|-~;".contains(_))
      ),
      SymbolDesc(
        hardKeywords = Set("import", "data"),
        hardOperators = Set("(", ")", "[", "]", "->"),
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

  lazy val fullParser: Parsley[List[Sourced[Token]]] = lexer.fully(Parsley.many(tokenParser))

  private lazy val tokenParser: Parsley[Sourced[Token]] =
    identifier <|> symbolParser <|> standaloneSymbolParser <|> keyword <|> integerLiteral

  private lazy val symbolParser: Parsley[Sourced[Token.Symbol]] = sourcedLexeme(
    lexer.nonlexeme.names.userDefinedOperator.map(Token.Symbol.apply)
  )

  private lazy val standaloneSymbolParser: Parsley[Sourced[Token.Symbol]] = sourcedLexeme(
    character.strings("(", ")", "[", "]", ",", "->").map(Token.Symbol.apply)
  ).label("special operator")

  private lazy val keyword: Parsley[Sourced[Token.Keyword]] = sourcedLexeme(
    character.strings("import", "data").map(Token.Keyword.apply)
  ).label("keyword")

  private lazy val integerLiteral: Parsley[Sourced[Token.IntegerLiteral]] = sourcedLexeme(
    lexer.nonlexeme.integer.decimal.map(Token.IntegerLiteral.apply)
  ).label("integer literal")

  private lazy val identifier: Parsley[Sourced[Token.Identifier]] = sourcedLexeme(
    lexer.nonlexeme.names.identifier.map(Token.Identifier.apply)
  )

  private lazy val position: Parsley[Position] = pos.map(Position.apply.tupled)

  private def sourcedLexeme[T](parser: Parsley[T]): Parsley[Sourced[T]] = for {
    from <- position
    t    <- markAsToken(parser)
    to   <- position
    _    <- lexer.space.whiteSpace
  } yield Sourced(file, PositionRange(from, to), t)
}
