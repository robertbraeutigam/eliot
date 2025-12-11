package com.vanillasource.eliot.eliotc.ast

import cats.Monad
import cats.effect.Sync
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.CompilationProcess
import com.vanillasource.eliot.eliotc.ast.ASTComponent.component
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.processor.OneToOneProcessor
import com.vanillasource.eliot.eliotc.source.error.SourcedError.registerCompilerError
import com.vanillasource.eliot.eliotc.source.pos.Sourced
import com.vanillasource.eliot.eliotc.token.SourceTokens
import com.vanillasource.parser.Parser.*
import com.vanillasource.parser.ParserError

class ASTParser[F[_]: {Monad, Sync}]
    extends OneToOneProcessor((key: SourceAST.Key) => SourceTokens.Key(key.file))
    with Logging {
  override def generateFromFact(sourceTokens: SourceTokens)(using process: CompilationProcess[F]): F[Unit] = {
    val tokens    = sourceTokens.tokens.value
    val file      = sourceTokens.file
    val astResult = component[AST].fully().parse(tokens)

    for {
      _ <- astResult.allErrors.map {
             case ParserError(pos, expected) if pos >= tokens.size =>
               tokens match {
                 case Nil =>
                   registerCompilerError(
                     sourceTokens.tokens.as(s"Expected ${expectedMessage(expected)}, but input was empty.")
                   )
                 case _   =>
                   val pos = tokens.last.range.to
                   registerCompilerError(
                     sourceTokens.tokens.as(s"Expected ${expectedMessage(expected)}, but end of input reached.")
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
               debug[F](s"Generated AST for $file: ${ast.show}.") >> process.registerFact(
                 SourceAST(file, sourceTokens.tokens.as(ast))
               )
             case None      => Monad[F].unit
    } yield ()
  }

  private def expectedMessage(expected: Set[String]): String = expected.toSeq match
    case Nil         => "nothing"
    case head :: Nil => head
    case _           => s"${expected.init.mkString(", ")} or ${expected.last}"
}
