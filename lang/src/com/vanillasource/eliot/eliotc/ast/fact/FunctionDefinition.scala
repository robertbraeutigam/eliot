package com.vanillasource.eliot.eliotc.ast.fact

import cats.syntax.all.*
import cats.{Eq, Show}
import ASTComponent.component
import Primitives.*
import com.vanillasource.eliot.eliotc.ast.*
import com.vanillasource.eliot.eliotc.ast.parser.Parser
import com.vanillasource.eliot.eliotc.module.fact.{QualifiedName, Qualifier}
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.token.Token
import Parser.*

case class FunctionDefinition(
    name: Sourced[QualifiedName],
    genericParameters: Seq[GenericParameter],
    args: Seq[ArgumentDefinition],
    typeDefinition: Sourced[Expression],
    body: Option[Sourced[Expression]], // Can be empty for abstract functions
    fixity: Fixity = Fixity.Application,
    precedence: Seq[PrecedenceDeclaration] = Seq.empty,
    visibility: Visibility = Visibility.Public,
    // When true, the type checker treats this definition as a stuck, identity-based reference and does NOT unfold its
    // body during checking (later phases, e.g. backend representation lowering, may still unfold it). Lets a type with
    // a body — like a platform `opaque type Int[MIN, MAX] = <repr>` — stay distinct per type argument so range
    // assignability stays sound, instead of collapsing to its body.
    opaque: Boolean = false,
    // The `/** ... */` documentation comment preceding this definition, if any. Attached by source-position adjacency
    // in `ASTParser` and consumed only by the apidoc tooling; never read by the compiler proper, never part of
    // `signatureEquality` (layers may document the same name differently), and dropped at the core boundary.
    doc: Option[Sourced[String]] = None,
    // The effect abilities this definition *discharges* — the negative `{…, -E}` members of its effect set
    // (docs/effect-discharge-accounting.md). Populated by `EffectSugarDesugarer` from the signature's `EffectfulType`
    // negatives (as bare ability names, resolved to `AbilityFQN` in the resolve phase); empty for every ordinary
    // definition. Rides the same fact chain as `opaque`, ending on `OperatorResolvedValue`, where the effect
    // accounting subtracts it from a caller's used-effect set. Included in `signatureEquality` so a layer may not
    // silently disagree with the abstract declaration about what it discharges.
    dischargedEffects: Seq[Sourced[String]] = Seq.empty
)

object FunctionDefinition {
  val signatureEquality: Eq[FunctionDefinition] = (x: FunctionDefinition, y: FunctionDefinition) =>
    x.genericParameters.length === y.genericParameters.length &&
      x.args.length === y.args.length &&
      (x.genericParameters zip y.genericParameters).forall(GenericParameter.signatureEquality.eqv) &&
      (x.args zip y.args).forall(ArgumentDefinition.signatureEquality.eqv) &&
      x.typeDefinition.value.show === y.typeDefinition.value.show

  given Show[FunctionDefinition] = (fd: FunctionDefinition) =>
    s"${fd.name.show}(${fd.args.map(_.show).mkString(", ")}): ${fd.body.show}"

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

  // `infix`/`prefix`/`postfix` (and likewise `private`/`opaque`, see `modifierPrefix`) are hard keywords (not
  // identifiers): besides marking fixity/visibility they are the only tokens that begin a (modified) definition, so
  // making them keywords lets a preceding greedily-parsed expression — a function *body*, or now a guarded *return
  // type* (effectful-signatures G2) — stop cleanly at the next definition rather than swallowing its modifier as an
  // application chain.
  private val fixityDecl: Parser[Sourced[Token], Fixity] =
    keyword("prefix").as(Fixity.Prefix: Fixity) or
      (keyword("infix") *> infixAssociativity).map(a => Fixity.Infix(a): Fixity) or
      keyword("postfix").as(Fixity.Postfix: Fixity)

  /** Parses the modifier prefix shared by `def` and `type` definitions:
    * `[opaque] [visibility] [fixity precedence*] <definitionKeyword>`, returning `(isOpaque, visibility, fixity,
    * precedence)`. Atomic, so it backtracks cleanly when `definitionKeyword` does not follow the modifiers (letting the
    * top-level `xor` dispatch try the next alternative). Called with `"def"` for value definitions and `"type"` for type
    * aliases — so a type alias can carry a fixity exactly like a `def`, e.g. `infix right type =>[A, B] = Function[A, B]`.
    */
  def modifierPrefix(
      definitionKeyword: String
  ): Parser[Sourced[Token], (Boolean, Visibility, Fixity, Seq[PrecedenceDeclaration])] = {
    val withFixity =
      (for {
        fixity <- fixityDecl
        prec   <- precedenceDeclaration.anyTimes()
        _      <- keyword(definitionKeyword)
      } yield (fixity, prec)).atomic()
    val plain      =
      keyword(definitionKeyword).map(_ => (Fixity.Application: Fixity, Seq.empty: Seq[PrecedenceDeclaration]))
    (for {
      isOpaque       <- keyword("opaque").as(true).optional().map(_.getOrElse(false))
      vis            <- component[Visibility].optional().map(_.getOrElse(Visibility.Public))
      (fixity, prec) <- withFixity or plain
    } yield (isOpaque, vis, fixity, prec)).atomic()
  }

  given ASTComponent[FunctionDefinition] = new ASTComponent[FunctionDefinition] {
    private val functionBody =
      (symbol("=") *> sourced(component[Expression])).optional()

    private val functionName: Parser[Sourced[Token], Sourced[Token]] =
      acceptIfAll(isIdentifier, isLowerCase)("function name") or acceptIf(isUserOperator, "function name")

    override val parser: Parser[Sourced[Token], FunctionDefinition] = for {
      (isOpaque, vis, fixity, prec) <- modifierPrefix("def")
      name                          <- functionName
      genericParameters             <- component[Seq[GenericParameter]]
      args                          <- optionalArgumentListOf(component[ArgumentDefinition])
      _                             <- symbol(":")
      typeExpression                <- sourced(Expression.typeRunParser)
      functionBody                  <- functionBody
    } yield FunctionDefinition(
      name.map(m => QualifiedName(m.content, Qualifier.Default)),
      genericParameters,
      args,
      typeExpression,
      functionBody,
      fixity,
      prec,
      vis,
      isOpaque
    )
  }
}
