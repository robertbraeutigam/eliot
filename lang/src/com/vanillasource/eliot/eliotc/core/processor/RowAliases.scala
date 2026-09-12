package com.vanillasource.eliot.eliotc.core.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.ast.fact.Expression
import com.vanillasource.eliot.eliotc.ast.fact.Expression.*
import com.vanillasource.eliot.eliotc.ast.fact.{FunctionDefinition, UnresolvedAbilityConstraint}
import com.vanillasource.eliot.eliotc.source.content.Sourced

/** The **row aliases** a file declares — a type alias whose body is an open effect row — and what a use of one
  * contributes to the definition naming it (`docs/effects.md` §2.4, §9.3 step 5).
  *
  * {{{
  * type Git[A] = {Process, FileSystem, Throw[IoError], Throw[GitError]} A
  *
  * def publishedTags(root: Path, id: PackageId): Git[List[TagRef]] = ...
  * }}}
  *
  * **The alias is an ordinary type alias, and a use of it an ordinary application.** `Git` lowers to
  * `type Git[A] = A` — the row is declaration metadata and never a type (§3.3), so it is erased from the alias's body
  * exactly as it is erased from a definition's return type — and `Git[List[TagRef]]` stays in the signature, where the
  * evaluator reduces it to `List[TagRef]` like any other alias application. What a use contributes is therefore not a
  * *type* at all but the alias's **row entries**, with the use's arguments substituted into them
  * ([[returnEntries]]): [[EffectSugarDesugarer]] mints one marked binding binder per entry and records them in the
  * definition's declared row, which is precisely the definition the user could have written out by hand — in
  * everything but the return type, which stays the name they did write.
  *
  * It was a **splice** until the binding binders carried their [[com.vanillasource.eliot.eliotc.ast.fact.GenericParameter.implementationMark]]
  * (landed 2026-09-11, `afd6d34c`): the write recognised a phantom binder by its occurring in no type, so binders
  * passed to an applied alias stopped reading as bindings and the row had to be β-reduced into the return type before
  * minting. With the mark that reason is gone, and only the entries — never the payload — cross the use site.
  *
  * **The alias mints no binders of its own**, and that is measured, not assumed (`docs/effects.md` §9.3 step 5's
  * correction). The plan had it mint them like any def, the use passing them as dead arguments that reduce away. But
  * an alias's parameters are its *value* args, so a mark minted there lands on an arrow domain — which
  * `BindingWriter.Writer.unmarked` does not erase, since it erases a *binder's* declared type — and the checker then
  * meets an ability of kind `Type -> Type` inside `Implementation[…]`. Recording the row on the alias's own
  * `effectRow` instead fails for a neighbouring reason: `ValueResolver.resolveEffectRow` resolves an entry's arguments
  * against the signature's *generic* params, and `type Fallible[E, A] = {Throw[E]} A` mentions a value arg. An alias
  * *names* a row; it does not perform one, so it has nothing to receive and nothing to be given.
  *
  * **Return position only.** A row means four different things by position — received at a return, *supplied* and
  * thunked at a parameter, the callback's own in an arrow codomain, stored and bound at construction in a `data`
  * field. An alias carries the row, not the placement discipline, and only the return case needs nothing of the slot:
  * a parameter would additionally have to thunk it, which is a rewrite of the slot rather than of the type naming it.
  * Every other position is therefore [[errors]], never a silent widening (§9.5, D18).
  *
  * **File-local**, because this runs at `core` and the module dictionary does not exist until `module` (one phase
  * later). An alias must be declared in the file that uses it, which is the same discipline the layer model already
  * imposes on every other name a file needs.
  */
object RowAliases {

  /** A type alias whose body is a top-level open effect row: its parameter names, the row's entries, and where it was
    * declared.
    */
  case class RowAlias(
      parameters: Seq[String],
      entries: Seq[UnresolvedAbilityConstraint[Sourced[Expression]]],
      declaredAt: Sourced[String]
  )

  /** The row aliases declared among these definitions, by name. */
  def declaredIn(functions: Seq[FunctionDefinition]): Map[String, RowAlias] =
    functions.flatMap { function =>
      for {
        body <- function.body if isTypeAlias(function)
        row  <- openRow(body)
      } yield function.name.value.name -> RowAlias(
        // A type alias's parameters are its *value* args: `TypeAliasDefinition` lowers `type Git[A]` to a
        // `Type`-returning function of one argument `A`, which is the types-are-values reading of `Git : Type -> Type`.
        function.args.map(_.name.value),
        row.effects,
        function.name.map(_.name)
      )
    }.toMap

  /** The row entries a definition receives from a row alias naming its **return type**, with the use's arguments
    * substituted into them — what [[EffectSugarDesugarer]] mints binders for, on top of any row the definition writes
    * out itself.
    *
    * The alias's *payload* is deliberately not substituted anywhere: the return type keeps the alias application the
    * user wrote, and the evaluator reduces it.
    */
  def returnEntries(
      aliases: Map[String, RowAlias],
      function: FunctionDefinition
  ): Seq[UnresolvedAbilityConstraint[Sourced[Expression]]] =
    if (aliases.isEmpty || isTypeAlias(function)) Seq.empty
    else
      aliasUse(aliases, function.typeDefinition) match {
        case Some((alias, arguments)) if arguments.size === alias.parameters.size =>
          alias.entries.map(substitute(alias.parameters.zip(arguments).toMap, _))
        case _                                                                    => Seq.empty
      }

  /** A row alias named where its row cannot be received: any position but a definition's own return type. */
  def errors(aliases: Map[String, RowAlias], function: FunctionDefinition): Seq[Sourced[String]] =
    if (aliases.isEmpty) Seq.empty
    else if (isTypeAlias(function)) {
      // A type alias naming a row alias (`type Held[A] = Option[Talk[A]]`, `type Loud = Talk`) would drop the row on
      // the floor: only a *definition's* return type receives one, and nothing here is one.
      function.body.toSeq.flatMap(positionErrors(aliases, _))
    } else {
      val returnUse    = aliasUse(aliases, function.typeDefinition)
      val arityError   = returnUse.toSeq.collect {
        case (alias, arguments) if arguments.size =!= alias.parameters.size =>
          function.typeDefinition.as(
            s"Row alias '${alias.declaredAt.value}' takes ${alias.parameters.size} type argument(s), " +
              s"but ${arguments.size} were given."
          )
      }
      // The return type's *nested* uses are not the return position, so they are reported like any other.
      val nestedReturn = if (returnUse.isDefined) Seq.empty else positionErrors(aliases, function.typeDefinition)
      val otherUses    =
        function.args.flatMap(arg => positionErrors(aliases, arg.typeExpression)) ++
          function.genericParameters.flatMap(gp => positionErrors(aliases, gp.typeRestriction))
      arityError ++ nestedReturn ++ otherUses
    }

  private def positionErrors(
      aliases: Map[String, RowAlias],
      expression: Sourced[Expression]
  ): Seq[Sourced[String]] =
    references(expression).filter(name => aliases.contains(name.value)).map { name =>
      name.as(
        s"Row alias '${name.value}' can only name a definition's return type. " +
          "A row on a parameter is supplied rather than received, so it must be written out."
      )
    }

  /** Whether this definition's declared return type is the bare `Type` — i.e. it is a type alias rather than a def. */
  private def isTypeAlias(function: FunctionDefinition): Boolean =
    function.typeDefinition.value match {
      case FunctionApplication(None, name, genericArguments, Seq()) if genericArguments.forall(_.isEmpty) =>
        name.value === "Type"
      case _                                                                                              => false
    }

  private def openRow(expression: Sourced[Expression]): Option[EffectfulType] =
    expression.value match {
      case row @ EffectfulType(_, _, None) => Some(row)
      case _                               => None
    }

  /** This expression read as a direct, fully-applied use of a row alias: the alias and its written type arguments. */
  private def aliasUse(
      aliases: Map[String, RowAlias],
      expression: Sourced[Expression]
  ): Option[(RowAlias, Seq[Sourced[Expression]])] =
    expression.value match {
      case FunctionApplication(None, name, genericArguments, Seq()) =>
        aliases.get(name.value).map(_ -> genericArguments.getOrElse(Seq.empty))
      case _                                                        => None
    }

  /** Every bare name referenced anywhere in a type expression. */
  private def references(expression: Sourced[Expression]): Seq[Sourced[String]] =
    expression.value match {
      case FunctionApplication(_, name, genericArguments, arguments) =>
        name +: (genericArguments.getOrElse(Seq.empty) ++ arguments).flatMap(references)
      case EffectfulType(effects, resultType, tail)                  =>
        effects.flatMap(_.typeArgs.flatMap(references)) ++ references(resultType) ++ tail.toSeq.flatMap(references)
      case WithBinding(subject, _)                                   => references(subject)
      case FlatExpression(parts)                                     => parts.flatMap(references)
      case _                                                         => Seq.empty
    }

  private def substitute(
      bindings: Map[String, Sourced[Expression]],
      constraint: UnresolvedAbilityConstraint[Sourced[Expression]]
  ): UnresolvedAbilityConstraint[Sourced[Expression]] =
    constraint.copy(typeArgs = constraint.typeArgs.map(substitute(bindings, _)))

  private def substitute(
      bindings: Map[String, Sourced[Expression]],
      expression: Sourced[Expression]
  ): Sourced[Expression] =
    expression.value match {
      case FunctionApplication(None, name, None, Seq()) if bindings.contains(name.value) =>
        bindings(name.value)
      case FunctionApplication(moduleName, name, genericArguments, arguments)            =>
        expression.as(
          FunctionApplication(
            moduleName,
            name,
            genericArguments.map(_.map(substitute(bindings, _))),
            arguments.map(substitute(bindings, _))
          )
        )
      case WithBinding(subject, implementation)                                          =>
        expression.as(WithBinding(substitute(bindings, subject), implementation))
      case FlatExpression(parts)                                                         =>
        expression.as(FlatExpression(parts.map(substitute(bindings, _))))
      case _                                                                             => expression
    }
}
