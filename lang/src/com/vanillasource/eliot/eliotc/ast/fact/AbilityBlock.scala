package com.vanillasource.eliot.eliotc.ast.fact

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.ast.fact.ASTComponent.component
import com.vanillasource.eliot.eliotc.ast.fact.Primitives.*
import com.vanillasource.eliot.eliotc.ast.parser.Parser.{
  acceptIfAll,
  between,
  optional,
  or,
  recoveringAnyTimes,
  recoveringAtLeastOnce
}
import com.vanillasource.eliot.eliotc.ast.parser.{Parser, ParserError}
import com.vanillasource.eliot.eliotc.module.fact.{QualifiedName, Qualifier}
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.token.Token

object AbilityBlock {
  val abilityBlock: ASTComponent[(Seq[ParserError], Seq[FunctionDefinition])] =
    new ASTComponent[(Seq[ParserError], Seq[FunctionDefinition])] {
      override val parser: Parser[Sourced[Token], (Seq[ParserError], Seq[FunctionDefinition])] =
        for {
          _                       <- keyword("ability")
          name                    <- acceptIfAll(isIdentifier, isUpperCase)("ability name")
          commonGenericParameters <- bracketedCommaSeparatedItems("[", component[GenericParameter], "]")
          (errors, functions)     <-
            (component[FunctionDefinition] or TypeAliasDefinition.typeAliasDefinition.parser)
              .recoveringAtLeastOnce(t => isKeyword(t) && (hasContent("def")(t) || hasContent("type")(t)))
              .between(symbol("{"), symbol("}"))
              .optional()
              .map(_.getOrElse(Seq.empty, Seq.empty))
        } yield {
          def gpTypeExpr(gp: GenericParameter): Sourced[Expression] =
            gp.name.as(Expression.FunctionApplication(None, gp.name, Seq.empty, Seq.empty))
          (
            errors,
            functions.map(f =>
              // Transform the function into an "ability" function. Change name into ability qualifier,
              // and also prepend the common generic parameters. Visibility is always public for ability functions.
              FunctionDefinition(
                f.name.map(n => QualifiedName(n.name, Qualifier.Ability(name.value.content))),
                commonGenericParameters ++ f.genericParameters,
                f.args,
                f.typeDefinition,
                f.body,
                visibility = Visibility.Public
              )
            ) :+
              // Synthetic marker that indicates the ability exists and encodes its generic parameters.
              // For `ability Foo[A, B]` we emit `Foo(arg0: A, arg1: B): A` — one argument per common
              // generic, mirroring the shape of implementation markers so marker-based dispatch stays
              // consistent across ability and implementation namespaces.
              FunctionDefinition(
                name.as(QualifiedName(name.value.content, Qualifier.Ability(name.value.content))),
                commonGenericParameters,
                commonGenericParameters.zipWithIndex.map { case (gp, i) =>
                  ArgumentDefinition(name.as(s"arg$i"), gpTypeExpr(gp))
                },
                gpTypeExpr(commonGenericParameters.head),
                None
              )
          )
        }
    }
}
