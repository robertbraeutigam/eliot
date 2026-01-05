package com.vanillasource.eliot.eliotc.ast

import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.ast.ASTComponent.component
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.TransformationProcessor
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerError
import com.vanillasource.eliot.eliotc.token.SourceTokens
import com.vanillasource.parser.Parser.*
import com.vanillasource.parser.ParserError

class ASTParser
    extends TransformationProcessor[SourceAST, SourceTokens, SourceTokens.Key, SourceAST.Key]
    with Logging {

  override protected def getInputKey(outputKey: SourceAST.Key): SourceTokens.Key =
    SourceTokens.Key(outputKey.file)

  override protected def generateFromKeyAndFact(key: SourceAST.Key, sourceTokens: SourceTokens): CompilerIO[SourceAST] = {
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
      ast <- astResult.value match
               case Some(ast) =>
                 debug[CompilerIO](s"Generated AST for $file: ${ast.show}.").as(ast)
               case None      => abort[AST]
    } yield SourceAST(file, sourceTokens.tokens.as(ast))
  }

  private def expectedMessage(expected: Set[String]): String = expected.toSeq match
    case Nil         => "nothing"
    case head :: Nil => head
    case _           => s"${expected.init.mkString(", ")} or ${expected.last}"
}
