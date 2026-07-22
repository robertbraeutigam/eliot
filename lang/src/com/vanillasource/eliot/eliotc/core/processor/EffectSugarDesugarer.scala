package com.vanillasource.eliot.eliotc.core.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.ast.fact.{DataDefinition, EffectRow, FunctionDefinition, GenericParameter}
import com.vanillasource.eliot.eliotc.ast.fact.Expression
import com.vanillasource.eliot.eliotc.ast.fact.Expression.*
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

  /** Rewrite a `data` definition's constructor field rows. The sanctioned form for a stored row is the **pinned** one
    * (`data TestCase(body: {Throw[E] | Id} Unit)`): a stored value must commit to one concrete representation, and a
    * pinned row *is* that representation spelled in effect vocabulary — the field rewrites to the canonical carrier
    * stack (`ThrowCarrier[E, Id, Unit]`) with no generic parameter introduced, keeping the data type itself
    * non-generic. An *open* positive row in a field is a user error (reported via [[rowErrors]]); as error recovery it
    * still lowers by the pre-pinned-rows rule — one shared inferable carrier `F[_]` **lifted onto the data type's own
    * generic parameters**, every field `{…} A` rewritten to `F[A]` — so downstream phases see a well-formed value.
    * Returns the definition unchanged when no field carries an effect row.
    *
    * This must run *before* [[DataDefinitionDesugarer]] splits the data into its type-constructor, value-constructor,
    * and accessor functions: those all thread `genericParameters` through uniformly, so a recovery-lifted carrier has a
    * home on the *type*. Desugaring the split functions instead would only add `F` to the value constructor, leaving
    * the type constructor nullary and `F` an unbound free variable — the field references a carrier the type cannot
    * record.
    */
  def desugar(data: DataDefinition, effectChannel: Boolean): DataDefinition = {
    // Effects-as-channel (docs/effects-as-channel.md §4): a *pinned* field row is an ordinary concrete carrier stack and
    // desugars identically in both paths, and an *open* field row is a user error either way (the stored-row rule
    // stays). So the data-field desugar is unchanged under the flag; only the recovery lowering of the (already
    // erroneous) open field row differs, and we keep it as-is to stay a well-formed value downstream.
    val _ = effectChannel
    val fieldExprs = data.constructors.getOrElse(Seq.empty).flatMap(_.fields.map(_.typeExpression))
    val rows       = fieldExprs.flatMap(collectRows).map(_.value)
    val openRows   = rows.filter(_.tail.isEmpty)
    val positives  = openRows.flatMap(_.effects).distinctBy(constraintKey)

    if (rows.isEmpty) data
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
      val rewriteExpr    = rewrite(carrierNameOpt, stripOpen = false)

      data.copy(
        genericParameters = carrierParam.toSeq ++ data.genericParameters,
        constructors = data.constructors.map(_.map { ctor =>
          ctor.copy(fields = ctor.fields.map(field => field.copy(typeExpression = rewriteExpr(field.typeExpression))))
        })
      )
    }
  }

  /** Rewrite a single function definition: collapse its `{…}` effect-sets onto one carrier. Returns the function
    * unchanged when it carries no effect rows. Synthetic functions (e.g. data-desugared ones) never carry `{…}`, so
    * applying this uniformly to every function is a no-op for them.
    */
  def desugar(function: FunctionDefinition, effectChannel: Boolean): FunctionDefinition =
    if (effectChannel) desugarChannel(function) else desugarCarrier(function)

  /** The default (carrier) desugar: collapse the signature's open `{…}` rows onto one inferable higher-kinded carrier
    * generic `F[_]`, adding one `F ~ Ei` constraint per positive entry and rewriting every open `{…} A` to `F[A]`. This
    * is the pre-effects-as-channel behaviour; unchanged unless `--effect-channel` is given.
    */
  private def desugarCarrier(function: FunctionDefinition): FunctionDefinition = {
    val signatureExprs =
      function.args.map(_.typeExpression) ++
        function.genericParameters.map(_.typeRestriction) :+
        function.typeDefinition
    val rows           = (signatureExprs ++ function.body.toSeq).flatMap(collectRows).map(_.value)
    val openRows       = rows.filter(_.tail.isEmpty)
    val positives      = openRows.flatMap(_.effects).distinctBy(constraintKey)

    if (rows.isEmpty) function
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
      val rewriteExpr    = rewrite(carrierNameOpt, stripOpen = false)

      function.copy(
        genericParameters = carrierParam.toSeq ++ function.genericParameters.map(gp =>
          gp.copy(typeRestriction = rewriteExpr(gp.typeRestriction))
        ),
        args = function.args.map(arg => arg.copy(typeExpression = rewriteExpr(arg.typeExpression))),
        typeDefinition = rewriteExpr(function.typeDefinition),
        body = function.body.map(rewriteExpr),
        // Effects-as-channel Phase 1 (dark): record the open rows, position-attributed, before the rewrite above erases
        // the `{…}` nodes. Inert on this path — nothing reads it yet; see [[EffectRow]].
        effectRow = declaredEffectRow(function)
      )
    }
  }

  /** The effects-as-channel desugar (docs/effects-as-channel.md §4, Phase 3, `--effect-channel`): make the signature
    * **effect-blind**. Two flag-gated transforms:
    *
    *   - **Strip open rows to payload.** An open `{E1, E2} A` rewrites to `A` — no carrier generic is minted and no
    *     `F ~ Ei` constraint is added; the declared row is preserved out-of-band in [[EffectRow]] ([[declaredEffectRow]],
    *     computed from the *original* signature so parameter positions align). A *pinned* row (`{Throw[E] | Id} A`) still
    *     desugars to its canonical carrier stack — it is an ordinary concrete type (the reification boundary), unchanged.
    *   - **Erase the effect-ability carrier.** An effect-ability *method* (`Console[F[_]].printLine : F[Unit]`) has its
    *     higher-kinded carrier binder dropped and every `F[X]` in its signature rewritten to the payload `X`, so the
    *     checker sees `printLine : String -> Unit`. The synthetic ability *marker* (the value named after the ability)
    *     is left untouched: its retained carrier binder is the queryable "this is an effect ability" signal the
    *     ability resolver and the downstream accounting/weaver read (see [[eraseAbilityCarrier]]).
    */
  private def desugarChannel(function: FunctionDefinition): FunctionDefinition = {
    val erased      = eraseAbilityCarrier(function)
    val rewriteExpr = rewrite(None, stripOpen = true)
    erased.copy(
      genericParameters =
        erased.genericParameters.map(gp => gp.copy(typeRestriction = rewriteExpr(gp.typeRestriction))),
      args = erased.args.map(arg => arg.copy(typeExpression = rewriteExpr(arg.typeExpression))),
      typeDefinition = rewriteExpr(erased.typeDefinition),
      body = erased.body.map(rewriteExpr),
      effectRow = declaredEffectRow(function)
    )
  }

  /** Carrier-erase an effect-ability *method* (effects-as-channel Phase 3): if `function` is a `Qualifier.Ability`
    * method carrying a higher-kinded carrier binder (`F[_]`), drop that binder and rewrite every `F[X]` in its
    * signature and body to the payload `X`. The synthetic ability *marker* (local name equal to the ability name) and
    * every non-ability function are returned unchanged — the marker deliberately keeps its carrier as the "effect
    * ability" signal downstream code queries.
    */
  private def eraseAbilityCarrier(function: FunctionDefinition): FunctionDefinition =
    abilityCarrierName(function) match {
      case Some(carrier) =>
        val eraseExpr = eraseCarrierApplications(carrier)
        function.copy(
          genericParameters = function.genericParameters.filterNot(_.name.value === carrier),
          args = function.args.map(arg => arg.copy(typeExpression = eraseExpr(arg.typeExpression))),
          typeDefinition = eraseExpr(function.typeDefinition),
          body = function.body.map(eraseExpr)
        )
      case None          => function
    }

  /** The higher-kinded carrier binder name of an effect-ability *method*, else `None`. A method is a `Qualifier.Ability`
    * value whose local name differs from the ability's own name (which is the synthetic marker); its carrier is the sole
    * higher-kinded (arrow-kinded, `F[_]`) generic binder. A first-order ability method (`Show[A].show`, no `F[_]`) has
    * no such binder and is left alone.
    */
  private def abilityCarrierName(function: FunctionDefinition): Option[String] =
    function.name.value.qualifier match {
      case Qualifier.Ability(abilityName) if function.name.value.name =!= abilityName =>
        function.genericParameters.find(isHigherKindedBinder).map(_.name.value)
      case _                                                                          => None
    }

  /** Whether a generic binder is higher-kinded (a carrier), i.e. its kind restriction is an arrow `Function[_, _]`. */
  private def isHigherKindedBinder(gp: GenericParameter): Boolean = gp.typeRestriction.value match {
    case FunctionApplication(_, name, Some(_), _) => name.value === "Function"
    case _                                        => false
  }

  /** Rewrite every application of the carrier `carrier[X]` down to its payload `X`, descending through the whole
    * expression: `F[Unit]` becomes `Unit`, `A => F[B]` becomes `A => B`. A bare (unapplied) carrier reference does not
    * occur in a carrier-headed ability signature, so none is expected; it is left untouched if present.
    */
  private def eraseCarrierApplications(carrier: String)(expr: Sourced[Expression]): Sourced[Expression] = {
    val recurse = eraseCarrierApplications(carrier)
    expr.value match {
      case FunctionApplication(_, name, Some(genArgs), _) if name.value === carrier && genArgs.nonEmpty =>
        recurse(genArgs.last)
      case FunctionApplication(moduleName, name, genArgs, args)                                          =>
        expr.as(FunctionApplication(moduleName, name, genArgs.map(_.map(recurse)), args.map(recurse)))
      case FunctionLiteral(parameters, body)                                                             =>
        expr.as(
          FunctionLiteral(parameters.map(p => p.copy(typeExpression = p.typeExpression.map(recurse))), recurse(body))
        )
      case FlatExpression(parts)                                                                         =>
        expr.as(FlatExpression(parts.map(recurse)))
      case EffectfulType(effects, resultType, tail)                                                      =>
        expr.as(EffectfulType(effects, recurse(resultType), tail.map(recurse)))
      case MatchExpression(scrutinee, cases)                                                             =>
        expr.as(MatchExpression(recurse(scrutinee), cases.map(c => c.copy(body = recurse(c.body)))))
      case BlockExpression(lines)                                                                        =>
        expr.as(BlockExpression(lines.map { line =>
          line.copy(
            binder = line.binder.map(b => b.copy(typeExpression = b.typeExpression.map(recurse))),
            expression = recurse(line.expression)
          )
        }))
      case _: IntegerLiteral | _: StringLiteral                                                          => expr
    }
  }

  /** The effects-as-channel **declared row** (docs/effects-as-channel.md §4, Phase 1): the *open* effect-row entries at
    * each signature position of `function`, captured before [[rewrite]] collapses the `{…}` nodes onto the carrier.
    * Return-position rows are the value's own ambient row; each value parameter carrying an open row (an effectful
    * callback like `action: A => {Effect} Unit`) is recorded with its positional index. Body rows and
    * generic-parameter-bound rows are deliberately excluded — the declared row is the value's public signature only.
    */
  private def declaredEffectRow(function: FunctionDefinition): EffectRow[GenericParameter.AbilityConstraint] =
    EffectRow(
      openRowEntries(function.typeDefinition),
      function.args.zipWithIndex.flatMap { case (arg, index) =>
        val entries = openRowEntries(arg.typeExpression)
        Option.when(entries.nonEmpty)(EffectRow.ParameterEffects(index, entries))
      }
    )

  /** The distinct *open*-row (`tail == None`) ability entries anywhere within one signature-position expression. */
  private def openRowEntries(expr: Sourced[Expression]): Seq[GenericParameter.AbilityConstraint] =
    collectRows(expr).map(_.value).filter(_.tail.isEmpty).flatMap(_.effects).distinctBy(constraintKey)

  /** Rewrites the effect-rows of an expression: an *open* `{…} A` node becomes `F[A]` (the carrier is always present
    * then — a row anywhere in the signature introduced it); a *pinned* node (`{Throw[E], State[S] | Id} A`) becomes its
    * canonical carrier stack `ThrowCarrier[E, StateCarrier[S, Id], A]` — each entry's carrier named by the
    * `<Ability>Carrier` convention (colocated with the ability, so it resolves wherever the ability does), entries
    * nesting left-to-right (leftmost outermost), the base after `|` at the bottom, and the result type as the outermost
    * layer's final argument. Descends through the whole expression so no [[EffectfulType]] survives.
    */
  private def rewrite(carrierName: Option[String], stripOpen: Boolean)(expr: Sourced[Expression]): Sourced[Expression] = {
    val recurse = rewrite(carrierName, stripOpen)
    expr.value match {
      case EffectfulType(effects, resultType, Some(tail)) if effects.nonEmpty =>
        val rewrittenTail = recurse(tail)
        val innerStack    = effects.drop(1).foldRight(rewrittenTail) { (e, acc) =>
          e.abilityName.as(
            FunctionApplication(
              None,
              e.abilityName.map(_ + "Carrier"),
              Some(e.typeParameters.map(recurse) :+ acc),
              Seq.empty
            )
          )
        }
        val head          = effects.head
        expr.as(
          FunctionApplication(
            None,
            head.abilityName.map(_ + "Carrier"),
            Some(head.typeParameters.map(recurse) :+ innerStack :+ recurse(resultType)),
            Seq.empty
          )
        )
      case EffectfulType(_, resultType, None) if stripOpen                    =>
        // Effects-as-channel (Phase 3): an open row has no carrier — it is stripped to its payload, the declared row
        // preserved separately in [[EffectRow]].
        recurse(resultType)
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
