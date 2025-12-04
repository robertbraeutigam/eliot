package com.vanillasource.eliot.eliotc.ast

import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.CompilationProcess
import com.vanillasource.eliot.eliotc.ast.ASTComponent.component
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.processor.OneToOneProcessor
import com.vanillasource.eliot.eliotc.source.error.SourcedError.registerCompilerError
import com.vanillasource.eliot.eliotc.source.pos.Sourced
import com.vanillasource.eliot.eliotc.token.{SourceTokens, Token}
import com.vanillasource.parser.Parser.*
import com.vanillasource.parser.ParserError

class ASTParser extends OneToOneProcessor((key: SourceAST.Key) => SourceTokens.Key(key.path)) with Logging {
  override def generateFromFact(sourceTokens: SourceTokens)(using process: CompilationProcess): IO[Unit] =
    for {
      asts <- sourceTokens.tokens.traverse(generateFromTokens).map(_.flatten)
      _    <- process
                .registerFact(SourceAST(sourceTokens.path, asts))
                .whenA(asts.length === sourceTokens.tokens.length)
    } yield ()

  private def generateFromTokens(
      sourcedTokens: Sourced[Seq[Sourced[Token]]]
  )(using CompilationProcess): IO[Option[Sourced[AST]]] = {
    val astResult = component[AST].fully().parse(sourcedTokens.value)

    for {
      _ <- astResult.allErrors.map {
             case ParserError(pos, expected) if pos >= sourcedTokens.value.size =>
               sourcedTokens.value match {
                 case Nil =>
                   registerCompilerError(
                     sourcedTokens.as(s"Expected ${expectedMessage(expected)}, but input was empty.")
                   )
                 case _   =>
                   val pos = sourcedTokens.value.last.range.to
                   registerCompilerError(
                     sourcedTokens.as(s"Expected ${expectedMessage(expected)}, but end of input reached.")
                   )
               }
             case ParserError(pos, expected)                                    =>
               val token = sourcedTokens.value.get(pos).get
               registerCompilerError(
                 token.map(_ => s"Expected ${expectedMessage(expected)}, but encountered ${token.value.show}.")
               )
           }.sequence_
    } yield {
      if (astResult.allErrors.nonEmpty) {
        None
      } else {
        astResult.value.map(sourcedTokens.as(_))
      }
    }
  }

  private def expectedMessage(expected: Set[String]): String = expected.toSeq match
    case Nil         => "nothing"
    case head :: Nil => head
    case _           => s"${expected.init.mkString(", ")} or ${expected.last}"
}
