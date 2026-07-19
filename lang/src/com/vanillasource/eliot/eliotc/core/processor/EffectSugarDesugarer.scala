package com.vanillasource.eliot.eliotc.core.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.ast.fact.{DataDefinition, FunctionDefinition, GenericParameter}
import com.vanillasource.eliot.eliotc.ast.fact.Expression
import com.vanillasource.eliot.eliotc.ast.fact.Expression.*
import com.vanillasource.eliot.eliotc.source.content.Sourced

/** Desugars the effect-set sugar `{ E1, E2, … } A` ([[Expression.EffectfulType]]) in a function signature into ordinary
  * higher-kinded-constrained generics, by one uniform rule:
  *
  *   - introduce **one** shared inferable higher-kinded carrier `F[_]` per signature (marked `auto`, so it counts
  *     toward `inferableArity` and is omittable at use sites),
  *   - for each distinct *positive* brace entry `Ei` add an ability constraint `F ~ Ei` (the carrier appended as `Ei`'s
  *     final type argument: `Suspend` becomes `Suspend[F]`, `State[Account]` becomes `State[Account, F]`),
  *   - rewrite every positive `{…} A` occurrence to `F[A]`.
  *
  * So `def readLine: {Suspend} String` becomes `def readLine[auto F[_] ~ Suspend]: F[String]`, identical (signature-wise) to
  * the hand-written form. No effect-specific classification or wrapping survives: after desugaring everything
  * downstream sees plain HKT-constrained generics. There is no body lifting here (that is the later `effect` phase) —
  * bodies must already be in monadic form; the rewrite still descends into the body so no [[Expression.EffectfulType]]
  * survives.
  *
  * **Negative members `-E`** (discharge-aware effect accounting, docs/effect-discharge-accounting.md) are handled
  * separately: they introduce no carrier constraint and are recorded (as bare ability names) in the function's
  * [[FunctionDefinition.dischargedEffects]]. A `{…} A` node made only of negatives introduces no carrier and passes its
  * inner type through unchanged (`{-Abort} G[A]` ⤳ `G[A]`) — this is how the explicit-carrier discharger primitives
  * annotate the effect they reify away. A mixed `{Console, -Abort} A` still wraps in the (Console) carrier and records
  * `Abort` as discharged.
  */
object EffectSugarDesugarer {

  /** Rewrite a `data` definition: collapse the *positive* `{…}` effect-sets in its constructor field types onto one
    * shared inferable carrier `F[_]` **lifted onto the data type's own generic parameters**, and rewrite every field
    * `{…} A` to `F[A]`. Returns the definition unchanged when no field carries an effect.
    *
    * This must run *before* [[DataDefinitionDesugarer]] splits the data into its type-constructor, value-constructor,
    * and accessor functions: those all thread `genericParameters` through uniformly, so lifting the carrier here gives
    * `F` a home on the *type* (`data TestCase(body: {Throw[E]} Unit)` ⤳ `data TestCase[auto F[_] ~ Throw[E]](body:
    * F[Unit])`). Desugaring the split functions instead would only add `F` to the value constructor, leaving the type
    * constructor nullary and `F` an unbound free variable — the field references a carrier the type cannot record.
    *
    * The carrier is `auto` (inferable), so use sites keep writing the bare type name; the carrier is inferred exactly
    * like any other `auto` generic. Negatives (`{-E}`) are not meaningful on a stored field (discharge is a *function*'s
    * behaviour, not a value's shape); a negatives-only field simply passes its inner type through, introducing no
    * carrier — same rule the function path uses.
    */
  def desugar(data: DataDefinition): DataDefinition = {
    val fieldExprs = data.constructors.getOrElse(Seq.empty).flatMap(_.fields.map(_.typeExpression.value))
    val positives  = fieldExprs.flatMap(collectEffects(_.effects)).distinctBy(constraintKey)
    val negatives  = fieldExprs.flatMap(collectEffects(_.negativeEffects)).distinctBy(constraintKey)

    if (positives.isEmpty && negatives.isEmpty) data
    else {
      val anchor         = data.name
      val carrierNameOpt =
        Option.when(positives.nonEmpty)(freshName("F", data.genericParameters.map(_.name.value).toSet))
      val carrierParam   = carrierNameOpt.map { carrierName =>
        val carrierRef = anchor.as(typeExpr(anchor.as(carrierName)))
        GenericParameter(
          anchor.as(carrierName),
          anchor.as(functionKind(anchor)),
          positives.map(e => GenericParameter.AbilityConstraint(e.abilityName, e.typeParameters :+ carrierRef)),
          inferable = true
        )
      }
      val rewriteExpr    = rewrite(carrierNameOpt)

      data.copy(
        genericParameters = carrierParam.toSeq ++ data.genericParameters,
        constructors = data.constructors.map(_.map { ctor =>
          ctor.copy(fields = ctor.fields.map(field => field.copy(typeExpression = rewriteExpr(field.typeExpression))))
        })
      )
    }
  }

  /** Rewrite a single function definition: collapse its *positive* `{…}` effect-sets onto one carrier and record its
    * *negative* members as discharged. Returns the function unchanged when it carries no effects of either sign.
    * Synthetic functions (e.g. data-desugared ones) never carry `{…}`, so applying this uniformly to every function is
    * a no-op for them.
    */
  def desugar(function: FunctionDefinition): FunctionDefinition = {
    val signatureExprs =
      function.args.map(_.typeExpression) ++
        function.genericParameters.map(_.typeRestriction) :+
        function.typeDefinition
    val allExprs       = signatureExprs.map(_.value) ++ function.body.toSeq.map(_.value)
    val positives      = allExprs.flatMap(collectEffects(_.effects)).distinctBy(constraintKey)
    val negatives      = allExprs.flatMap(collectEffects(_.negativeEffects)).distinctBy(constraintKey)
    val discharged     = negatives.map(_.abilityName).distinctBy(_.value)

    if (positives.isEmpty && negatives.isEmpty) function
    else {
      val anchor         = function.name
      val carrierNameOpt = Option.when(positives.nonEmpty)(freshName("F", function.genericParameters.map(_.name.value).toSet))
      val carrierParam   = carrierNameOpt.map { carrierName =>
        val carrierRef = anchor.as(typeExpr(anchor.as(carrierName)))
        GenericParameter(
          anchor.as(carrierName),
          anchor.as(functionKind(anchor)),
          positives.map(e => GenericParameter.AbilityConstraint(e.abilityName, e.typeParameters :+ carrierRef)),
          inferable = true
        )
      }
      val rewriteExpr    = rewrite(carrierNameOpt)

      function.copy(
        genericParameters = carrierParam.toSeq ++ function.genericParameters.map(gp =>
          gp.copy(typeRestriction = rewriteExpr(gp.typeRestriction))
        ),
        args = function.args.map(arg => arg.copy(typeExpression = rewriteExpr(arg.typeExpression))),
        typeDefinition = rewriteExpr(function.typeDefinition),
        body = function.body.map(rewriteExpr),
        dischargedEffects = function.dischargedEffects ++ discharged
      )
    }
  }

  /** Collects, in source order, one sign's ability members of every `{…}` within the expression — `select` picks the
    * positive (`_.effects`) or negative (`_.negativeEffects`) side.
    */
  private def collectEffects(
      select: EffectfulType => Seq[GenericParameter.AbilityConstraint]
  )(expr: Expression): Seq[GenericParameter.AbilityConstraint] = expr match {
    case et @ EffectfulType(_, _, resultType)         => select(et) ++ collectEffects(select)(resultType.value)
    case FunctionApplication(_, _, genericArgs, args) =>
      genericArgs.getOrElse(Seq.empty).flatMap(e => collectEffects(select)(e.value)) ++ args.flatMap(e =>
        collectEffects(select)(e.value)
      )
    case FunctionLiteral(parameters, body)            =>
      parameters.flatMap(_.typeExpression.toSeq.flatMap(e => collectEffects(select)(e.value))) ++ collectEffects(select)(body.value)
    case FlatExpression(parts)                        => parts.flatMap(e => collectEffects(select)(e.value))
    case MatchExpression(scrutinee, cases)            =>
      collectEffects(select)(scrutinee.value) ++ cases.flatMap(c => collectEffects(select)(c.body.value))
    case BlockExpression(lines)                       =>
      lines.flatMap(l =>
        l.binder.flatMap(_.typeExpression).toSeq.flatMap(t => collectEffects(select)(t.value)) ++
          collectEffects(select)(l.expression.value)
      )
    case _: IntegerLiteral | _: StringLiteral         => Seq.empty
  }

  /** Rewrites the effect-sets of an expression: a `{…} A` node with any *positive* member becomes `F[A]` (the carrier
    * is always present then — a positive anywhere in the signature introduced it); a *negatives-only* node passes its
    * inner type through unchanged (the discharge was already recorded, no carrier to wrap in). Descends through the
    * whole expression so no [[EffectfulType]] survives.
    */
  private def rewrite(carrierName: Option[String])(expr: Sourced[Expression]): Sourced[Expression] = expr.value match {
    case EffectfulType(effects, _, resultType) if effects.nonEmpty =>
      val name = carrierName.getOrElse(
        throw IllegalStateException(s"A positive effect set introduced no carrier: ${expr.value.show}")
      )
      expr.as(FunctionApplication(None, expr.as(name), Some(Seq(rewrite(carrierName)(resultType))), Seq.empty))
    case EffectfulType(_, _, resultType)                           =>
      rewrite(carrierName)(resultType)
    case FunctionApplication(moduleName, name, genericArgs, args)  =>
      expr.as(
        FunctionApplication(
          moduleName,
          name,
          genericArgs.map(_.map(rewrite(carrierName))),
          args.map(rewrite(carrierName))
        )
      )
    case FunctionLiteral(parameters, body)                         =>
      expr.as(
        FunctionLiteral(
          parameters.map(p => p.copy(typeExpression = p.typeExpression.map(rewrite(carrierName)))),
          rewrite(carrierName)(body)
        )
      )
    case FlatExpression(parts)                                     =>
      expr.as(FlatExpression(parts.map(rewrite(carrierName))))
    case MatchExpression(scrutinee, cases)                         =>
      expr.as(
        MatchExpression(
          rewrite(carrierName)(scrutinee),
          cases.map(c => c.copy(body = rewrite(carrierName)(c.body)))
        )
      )
    case BlockExpression(lines)                                    =>
      expr.as(BlockExpression(lines.map { line =>
        line.copy(
          binder = line.binder.map(b => b.copy(typeExpression = b.typeExpression.map(rewrite(carrierName)))),
          expression = rewrite(carrierName)(line.expression)
        )
      }))
    case _: IntegerLiteral | _: StringLiteral                      => expr
  }

  private def constraintKey(ac: GenericParameter.AbilityConstraint): String =
    ac.abilityName.value + "|" + ac.typeParameters.map(_.value.show).mkString(",")

  /** The kind `Function[Type, Type]` of a `[F[_]]` carrier, built exactly as the `[F[_]]` arity sugar produces it. */
  private def functionKind(anchor: Sourced[?]): Expression =
    typeExpr(
      anchor.as("Function"),
      Seq(anchor.as(typeExpr(anchor.as("Type"))), anchor.as(typeExpr(anchor.as("Type"))))
    )

  private def typeExpr(name: Sourced[String], genericArgs: Seq[Sourced[Expression]] = Seq.empty): Expression =
    FunctionApplication(None, name, Option.when(genericArgs.nonEmpty)(genericArgs), Seq.empty)

  private def freshName(base: String, existingNames: Set[String]): String =
    if (!existingNames.contains(base)) base
    else Iterator.from(0).map(i => s"$base$i").find(!existingNames.contains(_)).get
}
