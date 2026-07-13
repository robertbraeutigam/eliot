package com.vanillasource.eliot.eliotc.core.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.ast.fact.{FunctionDefinition, Expression as SourceExpression}
import com.vanillasource.eliot.eliotc.core.fact.RoleHint
import com.vanillasource.eliot.eliotc.module.fact.{QualifiedName, Qualifier, WellKnownTypes}
import com.vanillasource.eliot.eliotc.source.content.Sourced

/** Desugars a def's `where <predicate>` refinement precondition into its **`^Where` companion** (bounds-as-refinements
  * §4.3) — the `Bool` function the refinement channel evaluates at each call site to demand the precondition.
  *
  * {{{
  *   def useByte(x: Int): Int where withinByte(range(x))
  *     ⟶  useByte$Where(x: Int$Meta): Bool = withinByte(range(x))
  * }}}
  *
  * Like the `^Meta` transfer companion ([[MetaTransferDesugarer]]) it lives in the [[Qualifier.Meta]] namespace and its
  * parameter types are the *meta* structures of the originals — the pure name transform `Int` ⤳ `Int$Meta`
  * ([[MetaConstructorDesugarer.metaTypeSuffix]]) — so inside the predicate a parameter `x` denotes its `Int$Meta` value
  * and `range(x)` projects the tracked range (an `Interval`). Distinct from the transfer companion by the extra
  * [[whereSuffix]] on the name, so a def may carry both a return brace and a `where`. The body is the predicate itself,
  * unwrapped (a `Bool`), and the declared return type is `Bool` written module-qualified so it resolves without the def
  * importing `Bool` (mirroring [[SourceExpression.trueReference]]).
  *
  * A parameter whose type has no meta structure (an untracked type, or a bare generic) has no `T$Meta`, so its retyped
  * reference does not resolve and the companion fails to compile — a loud error at the precondition rather than a
  * silently-unenforced `where` (per the gaps-must-be-fail-safe rule). For now the tracked type is `Int`, so a `where`
  * ranges over `Int` parameters; broadening to more domains is the second-domain work (§7). Companions are
  * compiler-pool-only (the channel evaluates them), dead in the runtime pool, never code-generated.
  */
object MetaWhereDesugarer {

  /** Appended to the def's name (in the [[Qualifier.Meta]] namespace) to form its `^Where` companion's name, keeping it
    * distinct from the `^Meta` transfer companion (which is the bare name). `$` is not an identifier character, so the
    * result cannot collide with a user-declared name.
    */
  val whereSuffix: String = "$Where"

  /** The `^Where` companion generated from `definition`'s `where` clause, or empty when it has none (every ordinary
    * def).
    */
  def desugar(definition: FunctionDefinition): Seq[(FunctionDefinition, RoleHint)] =
    definition.whereClause.toSeq.map(predicate => whereCompanion(definition, predicate) -> RoleHint.NoHint)

  private def whereCompanion(f: FunctionDefinition, predicate: Sourced[SourceExpression]): FunctionDefinition =
    FunctionDefinition(
      f.name.map(qn => QualifiedName(qn.name + whereSuffix, Qualifier.Meta)),
      Seq.empty,
      f.args.map(arg => arg.copy(typeExpression = metaTypeRef(arg.typeExpression))),
      boolTypeRef(f.name),
      Some(predicate),
      visibility = f.visibility
    )

  /** The meta *type* reference for a value type expression: the head type's name suffixed `$Meta`, bare (no args), the
    * same transform [[MetaTransferDesugarer]] uses. A non-application head is left unchanged (it will not resolve to a
    * meta structure, surfacing the limitation above as a loud error rather than a silent skip).
    */
  private def metaTypeRef(typeExpr: Sourced[SourceExpression]): Sourced[SourceExpression] =
    typeExpr.value match {
      case SourceExpression.FunctionApplication(module, fnName, _, _) =>
        typeExpr.as(
          SourceExpression.FunctionApplication(module, fnName.map(_ + MetaConstructorDesugarer.metaTypeSuffix), None, Seq.empty)
        )
      case _                                                         => typeExpr
    }

  /** A module-qualified reference to the `Bool` type (`eliot.lang.Bool::Bool`), so the companion's return type resolves
    * without the originating file importing `Bool`.
    */
  private def boolTypeRef(at: Sourced[?]): Sourced[SourceExpression] =
    at.as(
      SourceExpression.FunctionApplication(
        Some(at.as(WellKnownTypes.boolFQN.moduleName.show)),
        at.as(WellKnownTypes.boolFQN.name.name),
        None,
        Seq.empty
      )
    )
}
