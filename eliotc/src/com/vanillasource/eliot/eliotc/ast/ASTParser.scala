package com.vanillasource.eliot.eliotc.ast

import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.ast.TokenParser.{astParser, given}
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.source.{PositionRange, Sourced}
import com.vanillasource.eliot.eliotc.source.SourcedError.compilerError
import com.vanillasource.eliot.eliotc.token.{SourceTokens, Token}
import com.vanillasource.eliot.eliotc.{CompilationProcess, CompilerFact, CompilerProcessor}
import com.vanillasource.parser.Parser.*
import com.vanillasource.parser.ParserError

import java.io.File
import scala.collection.immutable.{AbstractSeq, LinearSeq}

class ASTParser extends CompilerProcessor with Logging {
  override def process(fact: CompilerFact)(using CompilationProcess): IO[Unit] = fact match {
    case SourceTokens(file, tokens) => parseAST(file, tokens)
    case _                          => IO.unit
  }

  private def parseAST(file: File, tokens: Seq[Sourced[Token]])(using process: CompilationProcess): IO[Unit] =
    astParser.parse(tokens) match
      case Left(p)    =>
        p match {
          case ParserError(Nil, expected)        =>
            tokens match {
              case Nil => compilerError(file, s"Expected ${expectedMessage(expected)}, but input was empty.")
              case _   =>
                val pos = tokens.last.range.to
                compilerError(
                  file,
                  Sourced(
                    PositionRange(pos, pos.next),
                    s"Expected ${expectedMessage(expected)}, but end of input reached."
                  )
                )
            }
          case ParserError(token :: _, expected) =>
            compilerError(
              file,
              token.map(_ => s"Expected ${expectedMessage(expected)}, but encountered ${token.value.show}.")
            )
        }
      case Right(ast) => debug(s"generated AST: $ast") >> process.registerFact(SourceAST(file, ast))

  private def expectedMessage(expected: Seq[String]): String = expected match
    case Nil         => "nothing"
    case head :: Nil => head
    case _           => s"${expected.init.mkString(", ")} or ${expected.last}"
}
