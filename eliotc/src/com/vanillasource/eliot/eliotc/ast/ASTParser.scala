package com.vanillasource.eliot.eliotc.ast

import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.ast.TokenParser.astParser
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.source.Sourced
import com.vanillasource.eliot.eliotc.source.SourcedError.compilerError
import com.vanillasource.eliot.eliotc.token.{SourceTokens, Token}
import com.vanillasource.eliot.eliotc.{CompilationProcess, CompilerFact, CompilerProcessor}
import com.vanillasource.parser.Parser.*
import com.vanillasource.parser.ParserError

import java.io.File

class ASTParser extends CompilerProcessor with Logging {
  override def process(fact: CompilerFact)(using CompilationProcess): IO[Unit] = fact match {
    case SourceTokens(file, tokens) => parseAST(file, tokens)
    case _                          => IO.unit
  }

  private def parseAST(file: File, tokens: Seq[Sourced[Token]])(using process: CompilationProcess): IO[Unit] =
    astParser.parse(tokens) match
      case Left(p)    =>
        p match {
          case ParserError(token :: _, expected) => compilerError(file, token.map(_ => expected.mkString(", ")))
          case ParserError(_, expected)          => compilerError(file, expected.mkString(", "))
        }
      case Right(ast) => debug(s"generated AST: $ast") >> process.registerFact(SourceAST(file, ast))
}
