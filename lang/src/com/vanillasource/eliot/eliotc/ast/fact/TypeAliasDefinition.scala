package com.vanillasource.eliot.eliotc.ast.fact

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.ast.fact.ASTComponent.component
import com.vanillasource.eliot.eliotc.ast.fact.Primitives.*
import com.vanillasource.eliot.eliotc.ast.parser.Parser
import com.vanillasource.eliot.eliotc.ast.parser.Parser.{acceptIfAll, optional}
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.token.Token

object TypeAliasDefinition {
  val typeAliasDefinition: ASTComponent[FunctionDefinition] = new ASTComponent[FunctionDefinition] {
    override val parser: Parser[Sourced[Token], FunctionDefinition] = for {
      vis               <- component[Visibility].optional().map(_.getOrElse(Visibility.Public))
      _                 <- keyword("type")
      name              <- acceptIfAll(isIdentifier, isUpperCase)("type name")
      genericParameters <- component[Seq[GenericParameter]]
      _                 <- symbol("=")
      body              <- sourced(component[Expression])
    } yield {
      val args     = genericParameters.map(gp => ArgumentDefinition(gp.name, gp.typeRestriction))
      val typeExpr = name.as(Expression.FunctionApplication(None, name.map(_ => "Type"), Seq.empty, Seq.empty))
      FunctionDefinition(
        name.map(n => QualifiedName(n.content, Qualifier.Type)),
        Seq.empty,
        args,
        typeExpr,
        Some(body),
        visibility = vis
      )
    }
  }
}
