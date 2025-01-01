package com.vanillasource.eliot.eliotc.ast

import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.ast.TokenParser.{astParser, given}
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.source.SourcedError.registerCompilerError
import com.vanillasource.eliot.eliotc.source.{PositionRange, Sourced}
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

  private def parseAST(file: File, tokens: Seq[Sourced[Token]])(using process: CompilationProcess): IO[Unit] = {
    val astResult = astParser.parse(tokens)

    for {
      _ <- astResult.allErrors.map {
             case ParserError(pos, expected) if pos >= tokens.size =>
               tokens match {
                 case Nil => registerCompilerError(file, s"Expected ${expectedMessage(expected)}, but input was empty.")
                 case _   =>
                   val pos = tokens.last.range.to
                   registerCompilerError(
                     Sourced(
                       file,
                       PositionRange(pos, pos.next),
                       s"Expected ${expectedMessage(expected)}, but end of input reached."
                     )
                   )
               }
             case ParserError(pos, expected)                       =>
               val token = tokens.get(pos).get
               registerCompilerError(
                 token.map(_ => s"Expected ${expectedMessage(expected)}, but encountered ${token.value.show}.")
               )
           }.sequence_
      _ <- astResult.value match
             case Some(ast) =>
               debug(s"generated AST for $file: ${ast.show}") >> process.registerFact(SourceAST(file, ast))
             case None      => IO.unit
    } yield ()
  }

  private def expectedMessage(expected: Set[String]): String = expected.toSeq match
    case Nil         => "nothing"
    case head :: Nil => head
    case _           => s"${expected.init.mkString(", ")} or ${expected.last}"
}
