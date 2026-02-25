package com.vanillasource.eliot.eliotc.ast.fact

import cats.syntax.all.*
import cats.{Eq, Show}
import ASTComponent.component
import Primitives.*
import com.vanillasource.eliot.eliotc.ast.*
import com.vanillasource.eliot.eliotc.ast.parser.Parser
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.token.Token
import Parser.*

case class FunctionDefinition(
    name: Sourced[QualifiedName],
    genericParameters: Seq[GenericParameter],
    args: Seq[ArgumentDefinition],
    typeDefinition: TypeReference,
    body: Option[Sourced[Expression]], // Can be empty for abstract functions
    fixity: Fixity = Fixity.Prefix,
    precedence: Seq[PrecedenceDeclaration] = Seq.empty
)

object FunctionDefinition {
  val signatureEquality: Eq[FunctionDefinition] = (x: FunctionDefinition, y: FunctionDefinition) =>
    x.genericParameters.length === y.genericParameters.length &&
      x.args.length === y.args.length &&
      (x.genericParameters zip y.genericParameters).forall(GenericParameter.signatureEquality.eqv) &&
      (x.args zip y.args).forall(ArgumentDefinition.signatureEquality.eqv) &&
      TypeReference.signatureEquality.eqv(x.typeDefinition, y.typeDefinition)

  given Show[FunctionDefinition] = (fd: FunctionDefinition) =>
    s"${fd.name.show}(${fd.args.map(_.show).mkString(", ")}): ${fd.body.show}"

  given ASTComponent[FunctionDefinition] = new ASTComponent[FunctionDefinition] {
    private val functionBody =
      (symbol("=") *> sourced(component[Expression])).optional()

    private val targetName: Parser[Sourced[Token], Sourced[String]] =
      sourced(
        (acceptIfAll(isIdentifier, isLowerCase)("operator name") or acceptIf(isUserOperator, "operator name"))
          .map(_.value.content)
      )

    private val precedenceTargets: Parser[Sourced[Token], Seq[Sourced[String]]] =
      bracketedCommaSeparatedItems("(", targetName, ")") or targetName.map(Seq(_))

    private val precedenceRelation: Parser[Sourced[Token], PrecedenceDeclaration.Relation] =
      identifierWith("above").as(PrecedenceDeclaration.Relation.Above: PrecedenceDeclaration.Relation) or
        identifierWith("below").as(PrecedenceDeclaration.Relation.Below: PrecedenceDeclaration.Relation) or
        identifierWith("at").as(PrecedenceDeclaration.Relation.At: PrecedenceDeclaration.Relation)

    private val precedenceDeclaration: Parser[Sourced[Token], PrecedenceDeclaration] = for {
      relation <- precedenceRelation
      targets  <- precedenceTargets
    } yield PrecedenceDeclaration(relation, targets)

    private val infixAssociativity: Parser[Sourced[Token], Fixity.Associativity] =
      (identifierWith("left").as(Fixity.Associativity.Left: Fixity.Associativity) or
        identifierWith("right").as(Fixity.Associativity.Right: Fixity.Associativity) or
        identifierWith("none").as(Fixity.Associativity.None: Fixity.Associativity))
        .optional()
        .map(_.getOrElse(Fixity.Associativity.Left))

    private val fixityDecl: Parser[Sourced[Token], Fixity] =
      identifierWith("prefix").as(Fixity.Prefix: Fixity) or
        (identifierWith("infix") *> infixAssociativity).map(a => Fixity.Infix(a): Fixity) or
        identifierWith("postfix").as(Fixity.Postfix: Fixity)

    private val fixityWithDef: Parser[Sourced[Token], (Fixity, Seq[PrecedenceDeclaration])] =
      (for {
        fixity <- fixityDecl
        prec   <- precedenceDeclaration.anyTimes()
        _      <- keyword("def")
      } yield (fixity, prec)).atomic()

    private val plainDef: Parser[Sourced[Token], (Fixity, Seq[PrecedenceDeclaration])] =
      keyword("def").map(_ => (Fixity.Prefix: Fixity, Seq.empty: Seq[PrecedenceDeclaration]))

    private val functionName: Parser[Sourced[Token], Sourced[Token]] =
      acceptIfAll(isIdentifier, isLowerCase)("function name") or acceptIf(isUserOperator, "function name")

    override val parser: Parser[Sourced[Token], FunctionDefinition] = for {
      (fixity, prec) <- fixityWithDef or plainDef
      name              <- functionName
      genericParameters <- component[Seq[GenericParameter]]
      args              <- optionalArgumentListOf(component[ArgumentDefinition])
      _                 <- symbol(":")
      typeReference     <- component[TypeReference]
      functionBody      <- functionBody
    } yield FunctionDefinition(
      name.map(m => QualifiedName(m.content, Qualifier.Default)),
      genericParameters,
      args,
      typeReference,
      functionBody,
      fixity,
      prec
    )
  }
}
