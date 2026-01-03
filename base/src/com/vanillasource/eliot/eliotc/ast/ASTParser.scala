package com.vanillasource.eliot.eliotc.ast

import cats.Monad
import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.ast.ASTComponent.component
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.processor.CompilationProcess
import com.vanillasource.eliot.eliotc.processor.impl.OneToOneProcessor
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerError
import com.vanillasource.eliot.eliotc.token.SourceTokens
import com.vanillasource.parser.Parser.*
import com.vanillasource.parser.ParserError

class ASTParser extends OneToOneProcessor((key: SourceAST.Key) => SourceTokens.Key(key.file)) with Logging {
  override def generateFromFact(sourceTokens: SourceTokens): CompilerIO[Unit] = {
    val tokens    = sourceTokens.tokens.value
    val file      = sourceTokens.file
    val astResult = component[AST].fully().parse(tokens)

    for {
      _ <- astResult.allErrors.map {
             case ParserError(pos, expected) if pos >= tokens.size =>
               tokens match {
                 case Nil =>
                   compilerError(
                     sourceTokens.tokens.as(s"Expected ${expectedMessage(expected)}, but input was empty.")
                   )
                 case _   =>
                   val pos = tokens.last.range.to
                   compilerError(
                     sourceTokens.tokens.as(s"Expected ${expectedMessage(expected)}, but end of input reached.")
                   )
               }
             case ParserError(pos, expected)                       =>
               val token = tokens.get(pos).get
               compilerError(
                 token.map(_ => s"Expected ${expectedMessage(expected)}, but encountered ${token.value.show}.")
               )
           }.sequence_
      _ <- astResult.value match
             case Some(ast) =>
               debug[CompilerIO](s"Generated AST for $file: ${ast.show}.") >> registerFactIfClear(
                 SourceAST(file, sourceTokens.tokens.as(ast))
               )
             case None      => Monad[CompilerIO].unit
    } yield ()
  }

  private def expectedMessage(expected: Set[String]): String = expected.toSeq match
    case Nil         => "nothing"
    case head :: Nil => head
    case _           => s"${expected.init.mkString(", ")} or ${expected.last}"
}
