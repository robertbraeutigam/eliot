package com.vanillasource.eliot.eliotc.ast.fact

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.ast.fact.ASTComponent.component
import com.vanillasource.eliot.eliotc.ast.fact.Primitives.*
import com.vanillasource.eliot.eliotc.ast.parser.Parser
import com.vanillasource.eliot.eliotc.ast.parser.Parser.{acceptIfAll, optional}
import com.vanillasource.eliot.eliotc.module.fact.{QualifiedName, Qualifier}
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.token.Token

object TypeAliasDefinition {
  val typeAliasDefinition: ASTComponent[FunctionDefinition] = new ASTComponent[FunctionDefinition] {
    override val parser: Parser[Sourced[Token], FunctionDefinition] = for {
      isOpaque          <- keyword("opaque").as(true).optional().map(_.getOrElse(false))
      vis               <- component[Visibility].optional().map(_.getOrElse(Visibility.Public))
      _                 <- keyword("type")
      name              <- acceptIfAll(isIdentifier, isUpperCase)("type name")
      genericParameters <- component[Seq[GenericParameter]]
      // A type-alias body is a type position, so parse it with `typeRunParser` (an operator run of type atoms), NOT the
      // greedy full expression parser. The full parser would consume the following top-level definition's leading
      // `left`/`right`/… fixity identifiers as an application chain, silently dropping its fixity; `typeRunParser` stops
      // cleanly at the next definition because every definition-introducing token (`infix`/`prefix`/`postfix`/`def`/
      // `type`/…) is a hard keyword and so is not a type-atom start. This lets an alias body carry a bare type operator,
      // e.g. `type Pred = A => Bool`.
      body              <- (symbol("=") *> sourced(Expression.typeRunParser)).optional()
    } yield {
      val args     = genericParameters.map(gp => ArgumentDefinition(gp.name, gp.typeRestriction, gp.inferable))
      val typeExpr = name.as(Expression.FunctionApplication(None, name.map(_ => "Type"), None, Seq.empty))
      FunctionDefinition(
        name.map(n => QualifiedName(n.content, Qualifier.Type)),
        Seq.empty,
        args,
        typeExpr,
        body,
        visibility = vis,
        opaque = isOpaque
      )
    }
  }
}
