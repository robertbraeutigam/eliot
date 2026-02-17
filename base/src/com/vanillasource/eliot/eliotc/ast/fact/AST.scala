package com.vanillasource.eliot.eliotc.ast.fact

import cats.Show
import cats.syntax.all.*
import ASTComponent.component
import Primitives.*
import com.vanillasource.eliot.eliotc.ast.*
import com.vanillasource.eliot.eliotc.ast.parser.{Parser, ParserError}
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.token.Token
import Parser.*

case class AST(
    importStatements: Seq[ImportStatement],
    functionDefinitions: Seq[FunctionDefinition],
    typeDefinitions: Seq[DataDefinition]
)

object AST {
  given Show[AST] = (ast: AST) =>
    s"import statements: ${ast.importStatements
        .map(_.show)
        .mkString(", ")}, function definitions: ${ast.functionDefinitions
        .map(_.show)
        .mkString(", ")}, type definitions: ${ast.typeDefinitions.map(_.show).mkString(", ")}"

  given ASTComponent[AST] = new ASTComponent[AST] {
    override def parser: Parser[Sourced[Token], AST] = for {
      importStatements <-
        component[ImportStatement]
          .attemptPhraseTo(anyKeyword.void or endOfInput())
          .anyTimesWhile(keyword("import").find())
          .map(_.flatten)
      definitions      <-
        (component[FunctionDefinition] xor component[DataDefinition])
          .attemptPhraseTo(anyKeyword.void or endOfInput())
          .anyTimesWhile(any())
          .map(_.flatten)
    } yield AST(importStatements, definitions.flatMap(_.left.toSeq), definitions.flatMap(_.toSeq))
  }

  private def anyKeyword = acceptIf(isKeyword)

  def parse(tokens: Seq[Sourced[Token]]): (Seq[ParserError], AST) = {
    val blocks = topBlocks(tokens)

    val importStatementsResults =
      blocks
        .takeWhile(ts => isKeyword(ts.head) && ts.head.value.content === "import")
        .map(ts => parseWith(component[ImportStatement], ts))
    val definitionsResults      =
      blocks
        .dropWhile(ts => isKeyword(ts.head) && ts.head.value.content === "import")
        .map(ts => parseWith(component[FunctionDefinition] xor component[DataDefinition], ts))

    val errors              = importStatementsResults.flatMap(_._1) ++ definitionsResults.flatMap(_._1)
    val importStatements    = importStatementsResults.flatMap(_._2)
    val functionDefinitions = definitionsResults.flatMap(_._2).flatMap(_.left.toOption)
    val dataDefinition      = definitionsResults.flatMap(_._2).flatMap(_.toOption)

    (errors, AST(importStatements, functionDefinitions, dataDefinition))
  }

  private def parseWith[A](
      parser: Parser[Sourced[Token], A],
      ts: Seq[Sourced[Token]]
  ): (Seq[ParserError], Option[A]) = {
    val result = parser.fully().parse(ts)

    (Seq(result.currentError), result.value)
  }

  /** Splits the incoming tokens into the top-level blocks a source-file can have. Each of these blocks is then parsed
    * separately. This also means all of them can have a separate error.
    */
  private def topBlocks(tokens: Seq[Sourced[Token]]): Seq[Seq[Sourced[Token]]] =
    tokens.foldLeft(Seq.empty) { (acc, token) =>
      token.value match {
        case Token.Keyword(content) => acc :+ Seq(token)
        case _                      =>
          acc match
            case Nil => Seq(Seq(token))
            case _   => acc.init :+ (acc.last :+ token)
      }
    }
}
