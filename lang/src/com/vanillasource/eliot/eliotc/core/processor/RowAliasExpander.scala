package com.vanillasource.eliot.eliotc.core.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.ast.fact.Expression
import com.vanillasource.eliot.eliotc.ast.fact.Expression.*
import com.vanillasource.eliot.eliotc.ast.fact.{FunctionDefinition, UnresolvedAbilityConstraint}
import com.vanillasource.eliot.eliotc.source.content.Sourced

/** Expands a **row alias** — a type alias whose body is an open effect row — where it is used as a definition's
  * **return type**, by splicing the row in at the use site before [[EffectSugarDesugarer]] runs.
  *
  * {{{
  * type Git[A] = {Process, FileSystem, Throw[IoError], Throw[GitError]} A
  *
  * def publishedTags(root: Path, id: PackageId): Git[List[TagRef]] = ...
  * -- becomes, before any other core transformation:
  * def publishedTags(root: Path, id: PackageId): {Process, FileSystem, Throw[IoError], Throw[GitError]} List[TagRef]
  * }}}
  *
  * It is a **syntactic** expansion and deliberately nothing more: the result is the definition the user could have
  * written out by hand, so every phase downstream — the binder minting, the `row` phase's scope check, the checker,
  * codegen — sees exactly what it sees today and needs no knowledge that an alias existed.
  *
  * **Why not an application.** The natural reading is that `Git` is the type-level lambda `λI0…I3. λA. A` and the use
  * site η-expands: mint the definition's own binders and apply `Git` to them. That fails one phase later.
  * [[com.vanillasource.eliot.eliotc.row.BindingWriter]] recognises a phantom binder by its occurring in **no**
  * parameter and no return type, and it reads that syntactically, before monomorphization — so binders passed to
  * `Git[J0, J1, J2, J3, List[TagRef]]` are "mentioned", and the write stops seeing them as bindings. β-reducing the
  * application at the use site is what removes them again, and β-reducing it *here* is this expansion. Application
  * and expansion denote the same type; only the expanded form is visible to the pre-mono walk.
  *
  * **Return position only.** A row means four different things by position — received at a return, *supplied* and
  * thunked at a parameter, the callback's own in an arrow codomain, stored and bound at construction in a `data`
  * field. An alias carries the row, not the placement discipline, and only the return case is a plain splice: a
  * parameter would additionally have to thunk the slot to `Unit => A`, which is a rewrite of the slot rather than of
  * the type it names. Every other position is therefore [[errors]], never a silent widening.
  *
  * **File-local**, because this runs at `core` and the module dictionary does not exist until `module` (one phase
  * later). An alias must be declared in the file that uses it, which is the same discipline the layer model already
  * imposes on every other name a file needs.
  */
object RowAliasExpander {

  /** A type alias whose body is a top-level open effect row: its generic parameter names, and the row itself. */
  case class RowAlias(parameters: Seq[String], row: EffectfulType, declaredAt: Sourced[String])

  /** The row aliases declared among these definitions, by name. */
  def rowAliases(functions: Seq[FunctionDefinition]): Map[String, RowAlias] =
    functions.flatMap { function =>
      for {
        body <- function.body if returnsType(function)
        row  <- openRow(body)
      } yield function.name.value.name -> RowAlias(
        // A type alias's parameters are its *value* args: `TypeAliasDefinition` lowers `type Git[A]` to a
        // `Type`-returning function of one argument `A`, which is the types-are-values reading of `Git : Type -> Type`.
        function.args.map(_.name.value),
        row,
        function.name.map(_.name)
      )
    }.toMap

  /** Splice a row alias used as this definition's return type. Everything else is returned unchanged. */
  def expand(aliases: Map[String, RowAlias], function: FunctionDefinition): FunctionDefinition =
    if (aliases.isEmpty || returnsType(function)) function
    else
      aliasUse(aliases, function.typeDefinition) match {
        case Some((alias, arguments)) if arguments.size === alias.parameters.size =>
          function.copy(typeDefinition =
            function.typeDefinition.as(substitute(alias.parameters.zip(arguments).toMap, alias.row))
          )
        case _                                                                    => function
      }

  /** A row alias used where it cannot be spliced: any position but a definition's own return type. */
  def errors(aliases: Map[String, RowAlias], function: FunctionDefinition): Seq[Sourced[String]] =
    if (aliases.isEmpty) Seq.empty
    else {
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
  private def returnsType(function: FunctionDefinition): Boolean =
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

  private def substitute(bindings: Map[String, Sourced[Expression]], row: EffectfulType): EffectfulType =
    EffectfulType(
      row.effects.map(substitute(bindings, _)),
      substitute(bindings, row.resultType),
      row.tail.map(substitute(bindings, _))
    )

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
      case row: EffectfulType                                                            =>
        expression.as(substitute(bindings, row))
      case WithBinding(subject, implementation)                                          =>
        expression.as(WithBinding(substitute(bindings, subject), implementation))
      case FlatExpression(parts)                                                         =>
        expression.as(FlatExpression(parts.map(substitute(bindings, _))))
      case _                                                                             => expression
    }
}
