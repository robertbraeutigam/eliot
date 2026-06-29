package com.vanillasource.eliot.eliotc.apidoc.render

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.ast.fact.Expression.FunctionApplication
import com.vanillasource.eliot.eliotc.ast.fact.*
import com.vanillasource.eliot.eliotc.module.fact.Qualifier

/** Renders an AST declaration back to a single line of Eliot source text — the signature shown (and syntax-highlighted)
  * on a documentation page. It reconstructs the surface syntax the compiler erased: the `def`/`type` keyword from the
  * name's qualifier, fixity/precedence/visibility/`opaque` modifiers, generic-parameter brackets (including the
  * higher-kinded `F[_]` sugar, recovered from its `Function[Type, Type]` encoding), value-argument parentheses, and the
  * `data` constructor list. Bodies are intentionally omitted from `def` signatures — a doc page shows the contract, not
  * the implementation.
  */
object SignatureRenderer {

  def function(fn: FunctionDefinition): String = fn.name.value.qualifier match {
    case Qualifier.Type => typeDefinition(fn)
    case _              => valueDefinition(fn)
  }

  /** `ability Name[commonParams]` — the header line; methods are rendered separately as members. */
  def abilityHeader(name: String, commonParameters: Seq[GenericParameter]): String =
    s"ability $name${genericList(commonParameters)}"

  /** `implement[gp] Ability[patternTypes]` — reconstructed from the implementation marker function. */
  def implementation(abilityName: String, marker: FunctionDefinition): String = {
    val pattern = marker.args.map(_.typeExpression.value.show).mkString("[", ", ", "]")
    s"implement${genericList(marker.genericParameters)} $abilityName$pattern"
  }

  def data(dd: DataDefinition): String = {
    val prefix = visibilityPrefix(dd.visibility)
    val head   = s"${prefix}data ${dd.name.value}${genericList(dd.genericParameters)}"
    dd.constructors match {
      case None                                                                       => head
      case Some(Seq(single)) if single.name.value === dd.name.value && single.fields.nonEmpty =>
        s"$head${valueArgs(single.fields)}"
      case Some(constructors)                                                          =>
        s"$head = ${constructors.map(constructor).mkString(" | ")}"
    }
  }

  private def constructor(c: DataConstructor): String =
    if (c.fields.isEmpty) c.name.value else s"${c.name.value}${valueArgs(c.fields)}"

  private def valueDefinition(fn: FunctionDefinition): String = {
    val modifiers = visibilityPrefix(fn.visibility) + fixityPrefix(fn.fixity, fn.precedence)
    val args      = if (fn.args.isEmpty) "" else valueArgs(fn.args)
    s"${modifiers}def ${fn.name.value.name}${genericList(fn.genericParameters)}$args: ${fn.typeDefinition.value.show}"
  }

  private def typeDefinition(fn: FunctionDefinition): String = {
    val prefix = visibilityPrefix(fn.visibility) + (if (fn.opaque) "opaque " else "")
    // For a `type`, the parameters live in `args` (TypeAliasDefinition moves them there) and are bracketed with `[]`.
    val params = if (fn.args.isEmpty) "" else fn.args.map(typeParameter).mkString("[", ", ", "]")
    val body   = fn.body.map(b => s" = ${b.value.show}").getOrElse("")
    s"${prefix}type ${fn.name.value.name}$params$body"
  }

  private def visibilityPrefix(visibility: Visibility): String =
    visibility match {
      case Visibility.Private => "private "
      case Visibility.Public  => ""
    }

  private def fixityPrefix(fixity: Fixity, precedence: Seq[PrecedenceDeclaration]): String = {
    val fixityWord = fixity match {
      case Fixity.Application                          => ""
      case Fixity.Prefix                              => "prefix "
      case Fixity.Postfix                             => "postfix "
      case Fixity.Infix(Fixity.Associativity.Left)    => "infix left "
      case Fixity.Infix(Fixity.Associativity.Right)   => "infix right "
      case Fixity.Infix(Fixity.Associativity.None)    => "infix none "
    }
    fixityWord + precedence.map(precedenceDeclaration).mkString
  }

  private def precedenceDeclaration(p: PrecedenceDeclaration): String = {
    val relation = p.relation match {
      case PrecedenceDeclaration.Relation.Above => "above"
      case PrecedenceDeclaration.Relation.Below => "below"
      case PrecedenceDeclaration.Relation.At    => "at"
    }
    val targets = p.targets.map(_.value) match {
      case Seq(single) => single
      case many        => many.mkString("(", ", ", ")")
    }
    s"$relation $targets "
  }

  private def genericList(parameters: Seq[GenericParameter]): String =
    if (parameters.isEmpty) "" else parameters.map(genericParameter).mkString("[", ", ", "]")

  private def genericParameter(gp: GenericParameter): String = {
    val auto        = if (gp.inferable) "auto " else ""
    val constraints =
      if (gp.abilityConstraints.isEmpty) "" else " ~ " + gp.abilityConstraints.map(constraint(gp.name.value)).mkString(" & ")
    s"$auto${gp.name.value}${kindOrRestriction(gp.typeRestriction.value)}$constraints"
  }

  private def typeParameter(arg: ArgumentDefinition): String = {
    val auto = if (arg.inferable) "auto " else ""
    s"$auto${arg.name.value}${kindOrRestriction(arg.typeExpression.value)}"
  }

  private def constraint(defaultGeneric: String)(c: GenericParameter.AbilityConstraint): String =
    c.typeParameters.map(_.value.show) match {
      case Seq(single) if single === defaultGeneric => c.abilityName.value
      case Seq()                                    => c.abilityName.value
      case params                                   => s"${c.abilityName.value}${params.mkString("[", ", ", "]")}"
    }

  /** The text following a binder name: nothing for kind `Type`, the `[_, _]` sugar for a higher kind, otherwise an
    * explicit `: Restriction`.
    */
  private def kindOrRestriction(restriction: Expression): String =
    if (isType(restriction)) ""
    else kindBrackets(restriction).map(b => b).getOrElse(s": ${restriction.show}")

  private def isType(expr: Expression): Boolean = expr match {
    case FunctionApplication(None, name, genericArguments, arguments) =>
      name.value === "Type" && genericArguments.forall(_.isEmpty) && arguments.isEmpty
    case _                                                            => false
  }

  /** Recovers the `[_]`/`[_, _]`/`[_[_]]` arity sugar from the `Function[..]`-nested encoding produced by the parser;
    * `None` when the restriction is not a pure arity kind.
    */
  private def kindBrackets(expr: Expression): Option[String] = {
    def params(e: Expression): Option[Seq[Expression]] =
      if (isType(e)) Some(Seq.empty)
      else
        e match {
          case FunctionApplication(None, name, Some(Seq(param, rest)), arguments)
              if name.value === "Function" && arguments.isEmpty =>
            params(rest.value).map(param.value +: _)
          case _ => None
        }

    params(expr).filter(_.nonEmpty).map(_.map(p => s"_${kindBrackets(p).getOrElse("")}").mkString("[", ", ", "]"))
  }

  private def valueArgs(args: Seq[ArgumentDefinition]): String =
    args.map(a => s"${a.name.value}: ${a.typeExpression.value.show}").mkString("(", ", ", ")")
}
