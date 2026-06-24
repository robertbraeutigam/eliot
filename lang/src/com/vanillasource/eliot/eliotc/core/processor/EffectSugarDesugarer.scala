package com.vanillasource.eliot.eliotc.core.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.ast.fact.{FunctionDefinition, GenericParameter}
import com.vanillasource.eliot.eliotc.ast.fact.Expression
import com.vanillasource.eliot.eliotc.ast.fact.Expression.*
import com.vanillasource.eliot.eliotc.source.content.Sourced

/** Desugars the effect-set sugar `{ E1, E2, … } A` ([[Expression.EffectfulType]]) in a function signature into ordinary
  * higher-kinded-constrained generics, by one uniform rule:
  *
  *   - introduce **one** shared inferable higher-kinded carrier `F[_]` per signature (marked `auto`, so it counts
  *     toward `inferableArity` and is omittable at use sites),
  *   - for each distinct brace entry `Ei` add an ability constraint `F ~ Ei` (the carrier appended as `Ei`'s final type
  *     argument: `Sync` becomes `Sync[F]`, `State[Account]` becomes `State[Account, F]`),
  *   - rewrite every `{…} A` occurrence to `F[A]`.
  *
  * So `def readLine: {Sync} String` becomes `def readLine[auto F[_] ~ Sync]: F[String]`, identical (signature-wise) to
  * the hand-written form. No effect-specific classification or wrapping survives: after desugaring everything
  * downstream sees plain HKT-constrained generics. There is no body lifting here (that is the later `effect` phase) —
  * bodies must already be in monadic form; the rewrite still descends into the body so no [[Expression.EffectfulType]]
  * survives.
  */
object EffectSugarDesugarer {

  /** Rewrite a single function definition, collapsing all its `{…}` effect-sets onto one carrier. Returns the function
    * unchanged when it carries no effects. Synthetic functions (e.g. data-desugared ones) never carry `{…}`, so
    * applying this uniformly to every function is a no-op for them.
    */
  def desugar(function: FunctionDefinition): FunctionDefinition = {
    val signatureExprs =
      function.args.map(_.typeExpression) ++
        function.genericParameters.map(_.typeRestriction) :+
        function.typeDefinition
    val effects        =
      (signatureExprs.flatMap(e => collectEffects(e.value)) ++
        function.body.toSeq.flatMap(b => collectEffects(b.value)))
        .distinctBy(constraintKey)

    if (effects.isEmpty) function
    else {
      val anchor       = function.name
      val carrierName  = freshName("F", function.genericParameters.map(_.name.value).toSet)
      val carrierRef   = anchor.as(typeExpr(anchor.as(carrierName)))
      val carrierParam = GenericParameter(
        anchor.as(carrierName),
        anchor.as(functionKind(anchor)),
        effects.map(e => GenericParameter.AbilityConstraint(e.abilityName, e.typeParameters :+ carrierRef)),
        inferable = true
      )
      val rewriteExpr  = rewrite(carrierName)

      function.copy(
        genericParameters = carrierParam +: function.genericParameters.map(gp =>
          gp.copy(typeRestriction = rewriteExpr(gp.typeRestriction))
        ),
        args = function.args.map(arg => arg.copy(typeExpression = rewriteExpr(arg.typeExpression))),
        typeDefinition = rewriteExpr(function.typeDefinition),
        body = function.body.map(rewriteExpr)
      )
    }
  }

  /** Collects every effect (in source order) mentioned in any `{…}` within the expression. */
  private def collectEffects(expr: Expression): Seq[GenericParameter.AbilityConstraint] = expr match {
    case EffectfulType(effects, resultType)           => effects ++ collectEffects(resultType.value)
    case FunctionApplication(_, _, genericArgs, args) =>
      genericArgs.getOrElse(Seq.empty).flatMap(e => collectEffects(e.value)) ++ args.flatMap(e =>
        collectEffects(e.value)
      )
    case FunctionLiteral(parameters, body)            =>
      parameters.flatMap(_.typeExpression.toSeq.flatMap(e => collectEffects(e.value))) ++ collectEffects(body.value)
    case FlatExpression(parts)                        => parts.flatMap(e => collectEffects(e.value))
    case MatchExpression(scrutinee, cases)            =>
      collectEffects(scrutinee.value) ++ cases.flatMap(c => collectEffects(c.body.value))
    case BlockExpression(lines)                       =>
      lines.flatMap(l =>
        l.binder.flatMap(_.typeExpression).toSeq.flatMap(t => collectEffects(t.value)) ++
          collectEffects(l.expression.value)
      )
    case _: IntegerLiteral | _: StringLiteral         => Seq.empty
  }

  /** Replaces every `{…} A` with `F[A]` (where `F` is the carrier), descending through the whole expression. */
  private def rewrite(carrierName: String)(expr: Sourced[Expression]): Sourced[Expression] = expr.value match {
    case EffectfulType(_, resultType)                             =>
      expr.as(FunctionApplication(None, expr.as(carrierName), Some(Seq(rewrite(carrierName)(resultType))), Seq.empty))
    case FunctionApplication(moduleName, name, genericArgs, args) =>
      expr.as(
        FunctionApplication(
          moduleName,
          name,
          genericArgs.map(_.map(rewrite(carrierName))),
          args.map(rewrite(carrierName))
        )
      )
    case FunctionLiteral(parameters, body)                        =>
      expr.as(
        FunctionLiteral(
          parameters.map(p => p.copy(typeExpression = p.typeExpression.map(rewrite(carrierName)))),
          rewrite(carrierName)(body)
        )
      )
    case FlatExpression(parts)                                    =>
      expr.as(FlatExpression(parts.map(rewrite(carrierName))))
    case MatchExpression(scrutinee, cases)                        =>
      expr.as(
        MatchExpression(
          rewrite(carrierName)(scrutinee),
          cases.map(c => c.copy(body = rewrite(carrierName)(c.body)))
        )
      )
    case BlockExpression(lines)                                   =>
      expr.as(BlockExpression(lines.map { line =>
        line.copy(
          binder = line.binder.map(b => b.copy(typeExpression = b.typeExpression.map(rewrite(carrierName)))),
          expression = rewrite(carrierName)(line.expression)
        )
      }))
    case _: IntegerLiteral | _: StringLiteral                     => expr
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
