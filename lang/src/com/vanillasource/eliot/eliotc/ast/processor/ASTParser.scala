package com.vanillasource.eliot.eliotc.ast.processor

import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.ast.fact.ASTComponent.component
import com.vanillasource.eliot.eliotc.ast.fact.{AST, SourceAST}
import com.vanillasource.eliot.eliotc.ast.parser.ParserError
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.pos.Position
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
    val astResult = component[(Seq[ParserError], AST)].fully().parse(tokens)

    for {
      _   <- astResult.value
               .map(_._1)
               .getOrElse(Seq(astResult.currentError))
               .map {
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
               }
               .sequence_
      ast <- astResult.value.map(_._2) match {
               case Some(value) => debug[CompilerIO](s"Generated AST for $uri: ${value.show}.").as(value)
               case None        => abort
             }
    } yield SourceAST(uri, sourceTokens.tokens.as(attachDocComments(ast, sourceTokens.docComments)))
  }

  private def expectedMessage(expected: Set[String]): String = expected.toSeq match
    case Nil         => "nothing"
    case head :: Nil => head
    case _           => s"${expected.init.mkString(", ")} or ${expected.last}"

  /** Attach each documentation comment to the declaration it immediately precedes. A comment is bound to the
    * declaration with the smallest start position strictly after the comment (its nearest following declaration); when
    * several comments fall before the same declaration, the closest one wins. A trailing comment with no following
    * declaration is dropped. Comparison is position-based — primarily by line — so it is robust to indentation style.
    */
  private def attachDocComments(ast: AST, docComments: Seq[Sourced[String]]): AST = {
    if (docComments.isEmpty) return ast

    val declarationStarts: Seq[Position] =
      (ast.functionDefinitions.map(_.name.range.from) ++ ast.typeDefinitions.map(_.name.range.from))
        .sortBy(p => (p.line, p.col))

    def isAfter(a: Position, b: Position): Boolean = a.line > b.line || (a.line == b.line && a.col > b.col)

    val docByDeclaration: Map[Position, Sourced[String]] =
      docComments
        .flatMap(doc => declarationStarts.find(isAfter(_, doc.range.to)).map(_ -> doc))
        .groupBy(_._1)
        .view
        .mapValues(_.map(_._2).maxBy(d => (d.range.to.line, d.range.to.col)))
        .toMap

    ast.copy(
      functionDefinitions = ast.functionDefinitions.map(fd => fd.copy(doc = docByDeclaration.get(fd.name.range.from))),
      typeDefinitions = ast.typeDefinitions.map(dd => dd.copy(doc = docByDeclaration.get(dd.name.range.from)))
    )
  }
}
