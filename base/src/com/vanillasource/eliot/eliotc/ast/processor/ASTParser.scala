package com.vanillasource.eliot.eliotc.ast.processor

import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.ast.fact.ASTComponent.component
import com.vanillasource.eliot.eliotc.ast.fact.{AST, SourceAST}
import com.vanillasource.eliot.eliotc.ast.parser.ParserError
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.TransformationProcessor
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerError
import com.vanillasource.eliot.eliotc.token.SourceTokens
import com.vanillasource.eliot.eliotc.ast.parser.Parser.*

class ASTParser
    extends TransformationProcessor[SourceTokens.Key, SourceAST.Key](key => SourceTokens.Key(key.uri))
    with Logging {

  override protected def generateFromKeyAndFact(
      key: SourceAST.Key,
      sourceTokens: SourceTokens
  ): CompilerIO[SourceAST] = {
    val tokens    = sourceTokens.tokens.value
    val uri       = sourceTokens.uri
    val astResult = AST.parse(tokens)
    val ast       = astResult._2

    for {
      _ <- astResult._1.map {
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
      _ <- debug[CompilerIO](s"Generated AST for $uri: ${ast.show}.").as(ast)
    } yield SourceAST(uri, sourceTokens.tokens.as(ast))
  }

  private def expectedMessage(expected: Set[String]): String = expected.toSeq match
    case Nil         => "nothing"
    case head :: Nil => head
    case _           => s"${expected.init.mkString(", ")} or ${expected.last}"
}
