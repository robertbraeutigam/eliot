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
      vis               <- component[Visibility].optional().map(_.getOrElse(Visibility.Public))
      _                 <- keyword("type")
      name              <- acceptIfAll(isIdentifier, isUpperCase)("type name")
      genericParameters <- component[Seq[GenericParameter]]
      // A type-alias body is a type position, so parse it with the restricted `typeParser` (a single type atom), NOT
      // the greedy full expression parser: the full parser would consume the following top-level definition's leading
      // `infix`/`prefix`/`postfix`/`left`/… identifiers (they are not keywords) as an application chain, silently
      // dropping that definition's fixity. (Latent until an `infix` def followed a `type` alias with no plain `def`
      // between them — see `docs/int-min-max-plan.md`, Phase 5.)
      body              <- (symbol("=") *> sourced(Expression.typeParser)).optional()
    } yield {
      val args     = genericParameters.map(gp => ArgumentDefinition(gp.name, gp.typeRestriction))
      val typeExpr = name.as(Expression.FunctionApplication(None, name.map(_ => "Type"), Seq.empty, Seq.empty))
      FunctionDefinition(
        name.map(n => QualifiedName(n.content, Qualifier.Type)),
        Seq.empty,
        args,
        typeExpr,
        body,
        visibility = vis
      )
    }
  }
}
