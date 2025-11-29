package com.vanillasource.eliot.eliotc.ast

import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.ast.ASTComponent.component
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.source.error.SourcedError.registerCompilerError
import com.vanillasource.eliot.eliotc.token.{SourceTokens, Token}
import com.vanillasource.eliot.eliotc.{CompilationProcess, CompilerFact, CompilerFactKey, CompilerProcessor}
import com.vanillasource.parser.Parser.*
import com.vanillasource.parser.ParserError

import java.io.File
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.source.pos.{PositionRange, Sourced}

import java.nio.file.Path

class ASTParser extends CompilerProcessor with Logging {
  override def generate(factKey: CompilerFactKey)(using compilation: CompilationProcess): IO[Unit] = factKey match {
    case SourceAST.Key(path) =>
      compilation
        .getFact(SourceTokens.Key(path))
        .flatMap(_.traverse_(tokens => parseAST(tokens.path, tokens.rootPath, tokens.tokens)))
    case _                   => IO.unit
  }

  private def parseAST(path: Path, rootPath: Path, tokens: Seq[Sourced[Token]])(using
      process: CompilationProcess
  ): IO[Unit] = {
    val astResult = component[AST].fully().parse(tokens)
    val file      = rootPath.resolve(path).toFile

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
               debug(s"generated AST for $file: ${ast.show}") >> process.registerFact(SourceAST(path, rootPath, ast))
             case None      => IO.unit
    } yield ()
  }

  private def expectedMessage(expected: Set[String]): String = expected.toSeq match
    case Nil         => "nothing"
    case head :: Nil => head
    case _           => s"${expected.init.mkString(", ")} or ${expected.last}"
}
