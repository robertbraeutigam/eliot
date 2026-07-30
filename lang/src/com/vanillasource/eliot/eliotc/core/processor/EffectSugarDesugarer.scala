package com.vanillasource.eliot.eliotc.core.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.ast.fact.{DataDefinition, EffectRow, FunctionDefinition, GenericParameter}
import com.vanillasource.eliot.eliotc.ast.fact.Expression
import com.vanillasource.eliot.eliotc.ast.fact.Expression.*
import com.vanillasource.eliot.eliotc.effect.EffectCarrierNaming
import com.vanillasource.eliot.eliotc.effect.processor.EffectMachinery
import com.vanillasource.eliot.eliotc.module.fact.Qualifier
import com.vanillasource.eliot.eliotc.source.content.Sourced

/** Desugars the effect-row sugar `{ E1, E2, … } A` ([[Expression.EffectfulType]]), in its two forms.
  *
  * **Open rows** (`{E1, E2} A`, no tail) in a function signature become ordinary higher-kinded-constrained generics, by
  * one uniform rule:
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
  * **Pinned rows** (`{E1, E2 | T} A`, docs/effect-row-tails.md) are not constraints at all but *concrete types*: the
  * canonical carrier stack realizing exactly those effects over the base `T`. The rewrite is pure type application —
  * `{Throw[E], State[S] | Id} A` ⤳ `ThrowCarrier[E, StateCarrier[S, Id], A]` — with each entry's carrier named by the
  * `<Ability>Carrier` convention (colocated with its ability, so it resolves wherever the ability does), entries
  * nesting left-to-right (leftmost = outermost = discharged first), and no generic parameter introduced. This is the
  * one sanctioned spelling of a carrier stack: a stored (`data`-field) row must be pinned, and the discharger
  * signatures in the stdlib are written in it. An effect without a colocated `<Ability>Carrier` type (e.g. a
  * Suspend-riding one like `Console`) fails loudly at resolve time when pinned.
  *
  * Discharge carries no syntax here: a discharger's consumed effect vanishes structurally at the monomorphize-phase
  * residual check (it lands on an inner transformer carrier, never on the caller's ambient), so there is no `-E`
  * annotation to record.
  */
object EffectSugarDesugarer {

  /** Rewrite a `data` definition's constructor field rows — **the open ones only**. The sanctioned form for a stored
    * row is the **pinned** one (`data TestCase(body: {Throw[E] | Id} Unit)`): a stored value must commit to one
    * concrete representation, and a pinned row *is* that representation spelled in effect vocabulary. A pinned field
    * row is therefore left standing here and collapsed by the per-function pass ([[desugar(function:*]]) after
    * [[DataDefinitionDesugarer]] has split the data into functions — see below. An *open* positive row in a field is a
    * user error (reported via [[rowErrors]]); as error recovery it still lowers by the pre-pinned-rows rule — one
    * shared inferable carrier `F[_]` **lifted onto the data type's own generic parameters**, every open field `{…} A`
    * rewritten to `F[A]` — so downstream phases see a well-formed value. Returns the definition unchanged when no
    * field carries an *open* row.
    *
    * The recovery lift must run *before* [[DataDefinitionDesugarer]] splits the data: the split functions all thread
    * `genericParameters` through uniformly, so a recovery-lifted carrier has a home on the *type*. Desugaring the split
    * functions instead would only add `F` to the value constructor, leaving the type constructor nullary and `F` an
    * unbound free variable — the field references a carrier the type cannot record.
    *
    * A **pinned** row needs the opposite order, which is why the two are separated here. It introduces no generic
    * parameter, so it has nothing to place on the type — but it *is* a carrier-stack capture position, and the tag
    * recording that ([[EffectRow.pinnedParameterEffects]] on the value constructor,
    * [[EffectRow.returnPinnedEffects]] on the field accessor) is computed per *function*, by [[declaredEffectRow]],
    * from a signature position that is still spelled as an [[EffectfulType]]. Collapsing the field to its carrier
    * stack here erased that spelling before any function existed, so the constructor silently lost its pinned tag and
    * the row elaborator hoisted a stored computation instead of capturing it (docs/effects-as-rows.md §9 item 4).
    */
  def desugar(data: DataDefinition): DataDefinition = {
    val fieldExprs = data.constructors.getOrElse(Seq.empty).flatMap(_.fields.map(_.typeExpression))
    val rows       = fieldExprs.flatMap(collectRows).map(_.value)
    val openRows   = rows.filter(_.tail.isEmpty)
    val positives  = openRows.flatMap(_.effects).distinctBy(constraintKey)

    if (openRows.isEmpty) data
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
      val rewriteExpr    = rewrite(carrierNameOpt, rewritePinned = false)

      data.copy(
        genericParameters = carrierParam.toSeq ++ data.genericParameters,
        constructors = data.constructors.map(_.map { ctor =>
          ctor.copy(fields = ctor.fields.map(field => field.copy(typeExpression = rewriteExpr(field.typeExpression))))
        })
      )
    }
  }

  /** Rewrite a single function definition: collapse its open `{…}` rows onto one inferable higher-kinded carrier generic
    * `F[_]`, adding one `F ~ Ei` constraint per positive entry and rewriting every open `{…} A` to `F[A]`. Returns the
    * function unchanged when it carries no effect rows. Synthetic functions (e.g. data-desugared ones) never carry
    * `{…}`, so applying this uniformly to every function is a no-op for them.
    *
    * **The carrier is the signature's, not the row's** (decided 2026-07-28, docs/effects-as-rows.md §1 rule 2): when the
    * signature *already binds* exactly one effect carrier of its own (`G[_] ~ Effect`, as every discharger does), the
    * rows reuse **that** binder rather than minting a second one, and their entries join its constraints. `{Effect}`
    * then reads as "this signature's effect carrier" everywhere, which is what lets a discharger declare a slot that is
    * `G[A]` *and* row-tagged: `def else[G[_] ~ Effect, A](computation: {Abort | G} A, fallback: {Effect} A): G[A]` — the
    * same type it always had, now saying "a value or a computation" rather than "a computation". Without it the
    * fallback could only be spelled `G[A]`, which under §1 rule 2 no longer accepts a pure actual, and `host else
    * "localhost"` would have to be written `host else pure("localhost")` — pushing carrier machinery into user code.
    * The reuse is decidable from the declaration alone and applies to no signature written before it (nothing in the
    * tree mixed the two spellings); two or more `Effect`-constrained carriers decline it and mint as before.
    */
  def desugar(function: FunctionDefinition): FunctionDefinition = {
    val signatureExprs =
      function.args.map(_.typeExpression) ++
        function.genericParameters.map(_.typeRestriction) :+
        function.typeDefinition
    val rows           = (signatureExprs ++ function.body.toSeq).flatMap(collectRows).map(_.value)
    val openRows       = rows.filter(_.tail.isEmpty)
    val positives      = openRows.flatMap(_.effects).distinctBy(constraintKey)

    if (rows.isEmpty) function
    else {
      val anchor          = function.name
      val reusedCarrier   = ownEffectCarrier(function)
      val carrierNameOpt  = Option.when(positives.nonEmpty)(
        reusedCarrier.getOrElse(freshName("F", function.genericParameters.map(_.name.value).toSet))
      )
      val rowConstraints  = carrierNameOpt.toSeq.flatMap { carrierName =>
        val carrierRef = anchor.as(typeExpr(anchor.as(carrierName)))
        positives.map(e => GenericParameter.AbilityConstraint(e.abilityName, e.typeParameters :+ carrierRef))
      }
      // Minted only when nothing was reused: a reused binder keeps its declared position (and so its index, which
      // decides whether the elaborator can write it — A.11.4b's first-binder limit).
      val carrierParam    = carrierNameOpt.filterNot(reusedCarrier.contains).map { carrierName =>
        GenericParameter(anchor.as(carrierName), anchor.as(functionKind(anchor)), rowConstraints, inferable = true)
      }
      val rewriteExpr     = rewrite(carrierNameOpt)

      function.copy(
        genericParameters = carrierParam.toSeq ++ function.genericParameters.map { gp =>
          val constraints =
            if (reusedCarrier.contains(gp.name.value))
              (gp.abilityConstraints ++ rowConstraints).distinctBy(constraintKey)
            else gp.abilityConstraints
          gp.copy(typeRestriction = rewriteExpr(gp.typeRestriction), abilityConstraints = constraints)
        },
        args = function.args.map(arg => arg.copy(typeExpression = rewriteExpr(arg.typeExpression))),
        typeDefinition = rewriteExpr(function.typeDefinition),
        body = function.body.map(rewriteExpr),
        // Effects-as-channel Phase 1 (dark): record the open rows, position-attributed, before the rewrite above erases
        // the `{…}` nodes. Inert on this path — nothing reads it yet; see [[EffectRow]].
        effectRow = declaredEffectRow(function)
      )
    }
  }

  /** The one effect carrier a signature binds itself, if there is exactly one: a higher-kinded binder constrained by the
    * machinery ability `Effect` (`G[_] ~ Effect`), or — for an ability method — the ability's own binder
    * ([[abilityMethodCarrier]]). `None` when there is none, or more than one — the rule reuses only what is
    * unambiguous, and minting stays the fallback.
    */
  private def ownEffectCarrier(function: FunctionDefinition): Option[String] =
    function.genericParameters.filter(isEffectCarrierBinder) match {
      case Seq(single) => Some(single.name.value)
      case _           => abilityMethodCarrier(function)
    }

  /** The carrier an **ability method** declares its rows on: the ability's own higher-kinded binder, which
    * [[com.vanillasource.eliot.eliotc.ast.fact.AbilityBlock]] prepends to every method of the block. An ability method
    * spells its effects as a row (`ability Console[F[_]] { def printLine(s: String): {Console} Unit }`) exactly as an
    * ordinary definition does, and that row must land on the binder the *instance* binds — minting a second carrier
    * beside it would leave the ability's own binder unconstrained and unrelated to the method's result.
    *
    * This is the same reuse rule as an `Effect`-constrained binder above, reading a different declaration: there the
    * signature says "this is my carrier" with a constraint, here the enclosing `ability` block says it by binding it.
    * Ambiguity declines (the method binds a higher-kinded generic of its own, so which one is the ability's is no
    * longer a declaration but a guess) and minting stays the fallback — the fail-safe direction, since a minted
    * carrier is merely a second binder the checker must solve, never a wrong one written into the signature.
    */
  private def abilityMethodCarrier(function: FunctionDefinition): Option[String] =
    function.name.value.qualifier match {
      case Qualifier.Ability(_) =>
        function.genericParameters.filter(gp => isHigherKinded(gp.typeRestriction.value)) match {
          case Seq(single) => Some(single.name.value)
          case _           => None
        }
      case _                    => None
    }

  private def isEffectCarrierBinder(gp: GenericParameter): Boolean =
    isHigherKinded(gp.typeRestriction.value) &&
      gp.abilityConstraints.exists(_.abilityName.value == EffectMachinery.effectAbilityName)

  /** A binder's kind as written by the `[G[_]]` arity sugar: `Function[…]`, i.e. anything but `Type`. */
  private def isHigherKinded(kind: Expression): Boolean = kind match {
    case FunctionApplication(_, name, Some(_), _) => name.value == "Function"
    case _                                        => false
  }

  /** The effects-as-channel **declared row** (docs/effects-as-channel.md §4, Phase 1) plus the **carrier-stack
    * recognition tag** (finding 14 / §7 step 3): both captured from `function`'s signature before [[rewrite]] collapses
    * the `{…}` nodes.
    *
    *   - *Open* rows populate `returnEffects` / `parameterEffects`: return-position rows are the value's own ambient
    *     row; each value parameter carrying an open row (an effectful callback like `action: A => {Effect} Unit`) is
    *     recorded with its positional index.
    *   - *Pinned* rows populate `returnPinnedEffects` / `pinnedParameterEffects`: a signature position whose top-level
    *     type is a pinned row (`{Throw[E] | G} A`, which [[rewrite]] collapses to the canonical `<Ability>Carrier`
    *     stack) is a discharger/handler capture domain — recorded here *with its entries in declared (= discharge)
    *     order* (effects-as-rows R2, docs/effects-as-rows.md Appendix A.6), at the one point that knows it is a
    *     carrier stack, so no downstream phase re-derives carrier-ness — or the discharged entries — from shape or
    *     name. Entries are deliberately not deduplicated: a pinned row is a stack, and multiplicity/order matter.
    *
    * Body rows and generic-parameter-bound rows are deliberately excluded — the declared row is the value's public
    * signature only.
    */
  private def declaredEffectRow(function: FunctionDefinition): EffectRow[GenericParameter.AbilityConstraint] =
    EffectRow(
      openRowEntries(function.typeDefinition),
      function.args.zipWithIndex.flatMap { case (arg, index) =>
        val entries = openRowEntries(arg.typeExpression)
        Option.when(entries.nonEmpty)(EffectRow.ParameterEffects(index, entries))
      },
      returnPinnedEffects = pinnedRowEntries(function.typeDefinition),
      pinnedParameterEffects = function.args.zipWithIndex.flatMap { case (arg, index) =>
        val entries = pinnedRowEntries(arg.typeExpression)
        Option.when(entries.nonEmpty)(EffectRow.ParameterEffects(index, entries))
      }
    )

  /** The distinct *open*-row (`tail == None`) ability entries anywhere within one signature-position expression. */
  private def openRowEntries(expr: Sourced[Expression]): Seq[GenericParameter.AbilityConstraint] =
    collectRows(expr).map(_.value).filter(_.tail.isEmpty).flatMap(_.effects).distinctBy(constraintKey)

  /** The entries of a signature-position type expression that is *itself* — at top level — a **pinned** effect row
    * (`{Throw[E] | G} A`, i.e. an [[EffectfulType]] with a non-empty effect set and a base tail), in declared order.
    * Such a position collapses to a canonical carrier stack and is a discharger/handler capture domain (finding 14).
    * A *nested* pinned row (e.g. the codomain of a callback `A => {Throw[E] | G} B`) does not make the position itself
    * a carrier stack, so only the top-level shape counts. Empty for any non-pinned position.
    */
  private def pinnedRowEntries(expr: Sourced[Expression]): Seq[GenericParameter.AbilityConstraint] =
    expr.value match {
      case EffectfulType(effects, _, Some(_)) => effects
      case _                                  => Seq.empty
    }

  /** Rewrites the effect-rows of an expression: an *open* `{…} A` node becomes `F[A]` (the carrier is always present
    * then — a row anywhere in the signature introduced it); a *pinned* node (`{Throw[E], State[S] | Id} A`) becomes its
    * canonical carrier stack `ThrowCarrier[E, StateCarrier[S, Id], A]` — each entry's carrier named by the
    * `<Ability>Carrier` convention (colocated with the ability, so it resolves wherever the ability does), entries
    * nesting left-to-right (leftmost outermost), the base after `|` at the bottom, and the result type as the outermost
    * layer's final argument. Descends through the whole expression so no [[EffectfulType]] survives.
    *
    * @param rewritePinned
    *   `false` keeps each pinned node standing (still descending into its parts, so no *open* row survives) — the one
    *   caller that needs this is the `data`-level pass, which must leave a stored pinned row spelled as a row until
    *   the split functions exist to record its capture tag. Every other caller collapses both forms.
    */
  private def rewrite(carrierName: Option[String], rewritePinned: Boolean = true)(
      expr: Sourced[Expression]
  ): Sourced[Expression] = {
    val recurse = rewrite(carrierName, rewritePinned)
    expr.value match {
      case EffectfulType(effects, resultType, Some(tail)) if effects.nonEmpty && !rewritePinned =>
        expr.as(
          EffectfulType(
            effects.map(e => e.copy(typeParameters = e.typeParameters.map(recurse))),
            recurse(resultType),
            Some(recurse(tail))
          )
        )
      case EffectfulType(effects, resultType, Some(tail)) if effects.nonEmpty =>
        val rewrittenTail = recurse(tail)
        val innerStack    = effects.drop(1).foldRight(rewrittenTail) { (e, acc) =>
          e.abilityName.as(
            FunctionApplication(
              None,
              e.abilityName.map(EffectCarrierNaming.carrierName),
              Some(e.typeParameters.map(recurse) :+ acc),
              Seq.empty
            )
          )
        }
        val head          = effects.head
        expr.as(
          FunctionApplication(
            None,
            head.abilityName.map(EffectCarrierNaming.carrierName),
            Some(head.typeParameters.map(recurse) :+ innerStack :+ recurse(resultType)),
            Seq.empty
          )
        )
      case EffectfulType(effects, resultType, None) if effects.nonEmpty       =>
        val name = carrierName.getOrElse(
          throw IllegalStateException(s"An effect set introduced no carrier: ${expr.value.show}")
        )
        expr.as(FunctionApplication(None, expr.as(name), Some(Seq(recurse(resultType))), Seq.empty))
      case EffectfulType(_, resultType, _)                                    =>
        recurse(resultType)
      case FunctionApplication(moduleName, name, genericArgs, args)           =>
        expr.as(FunctionApplication(moduleName, name, genericArgs.map(_.map(recurse)), args.map(recurse)))
      case FunctionLiteral(parameters, body)                                  =>
        expr.as(
          FunctionLiteral(parameters.map(p => p.copy(typeExpression = p.typeExpression.map(recurse))), recurse(body))
        )
      case FlatExpression(parts)                                              =>
        expr.as(FlatExpression(parts.map(recurse)))
      case MatchExpression(scrutinee, cases)                                  =>
        expr.as(MatchExpression(recurse(scrutinee), cases.map(c => c.copy(body = recurse(c.body)))))
      case BlockExpression(lines)                                             =>
        expr.as(BlockExpression(lines.map { line =>
          line.copy(
            binder = line.binder.map(b => b.copy(typeExpression = b.typeExpression.map(recurse))),
            expression = recurse(line.expression)
          )
        }))
      case _: IntegerLiteral | _: StringLiteral                              => expr
    }
  }

  /** User-facing errors for effect rows in a `data` definition's constructor fields: an *open* row — a stored value
    * must commit to one concrete representation, so its row must name the base carrier it is realized over (be pinned).
    */
  def rowErrors(data: DataDefinition): Seq[Sourced[String]] = {
    val rows = data.constructors.getOrElse(Seq.empty).flatMap(_.fields.map(_.typeExpression)).flatMap(collectRows)
    rows.collect {
      case row if row.value.tail.isEmpty && row.value.effects.nonEmpty =>
        row.as("A stored effect row must be pinned to a base carrier, e.g. `{Throw[Error] | Id} String`.")
    }
  }

  /** Collects, in source order, every effect-row node within the expression, with its source position. */
  private def collectRows(expr: Sourced[Expression]): Seq[Sourced[EffectfulType]] = expr.value match {
    case et @ EffectfulType(effects, resultType, tail) =>
      (expr.as(et) +: effects.flatMap(_.typeParameters.flatMap(collectRows))) ++
        collectRows(resultType) ++ tail.toSeq.flatMap(collectRows)
    case FunctionApplication(_, _, genericArgs, args)             =>
      genericArgs.getOrElse(Seq.empty).flatMap(collectRows) ++ args.flatMap(collectRows)
    case FunctionLiteral(parameters, body)                        =>
      parameters.flatMap(_.typeExpression.toSeq.flatMap(collectRows)) ++ collectRows(body)
    case FlatExpression(parts)                                    => parts.flatMap(collectRows)
    case MatchExpression(scrutinee, cases)                        =>
      collectRows(scrutinee) ++ cases.flatMap(c => collectRows(c.body))
    case BlockExpression(lines)                                   =>
      lines.flatMap(l => l.binder.flatMap(_.typeExpression).toSeq.flatMap(collectRows) ++ collectRows(l.expression))
    case _: IntegerLiteral | _: StringLiteral                     => Seq.empty
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
