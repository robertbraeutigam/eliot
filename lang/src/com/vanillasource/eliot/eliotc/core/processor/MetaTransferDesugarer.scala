package com.vanillasource.eliot.eliotc.core.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.ast.fact.{FunctionDefinition, Expression as SourceExpression}
import com.vanillasource.eliot.eliotc.core.fact.RoleHint
import com.vanillasource.eliot.eliotc.module.fact.{QualifiedName, Qualifier}
import com.vanillasource.eliot.eliotc.source.content.Sourced

/** Desugars a def's return-position transfer brace into its `^Meta` **transfer companion** (bounds-as-refinements
  * §4.2, Step 4b) — the function the refinement channel evaluates to propagate meta through a call.
  *
  * {{{
  *   def add(a: Int, b: Int): Int {a.range + b.range}
  *     ⟶  add^Meta(a: Int$Meta, b: Int$Meta): Int$Meta = Int$Meta(a.range + b.range)
  * }}}
  *
  * The companion lives in the [[Qualifier.Meta]] namespace (so it never collides with `add`). How its parameter and
  * return types name their meta types is a pure **name transform** with **no cross-definition lookup**, split by whether
  * the def is generic:
  *   - a **monomorphic** vessel (`rangeAdd(a: Int, b: Int): Int {…}`) suffixes each concrete type to its meta structure
  *     `T$Meta` (the suffix [[MetaConstructorDesugarer.metaTypeSuffix]]) — `Int` ⤳ `Int$Meta`, which resolves because
  *     `Int` is slotted — and the channel reduces it at no type args.
  *   - a **generic** companion (`fold[A](condition: Bool, whenTrue: A, whenFalse: A): A {…}`) keeps its **original
  *     signature** verbatim: you cannot spell "the meta of `A`" (`A$Meta` is a different, undeclared name), so instead
  *     the channel reduces the companion at the *meta* type argument (`A := Int$Meta`), binding a bare `A` param straight
  *     to the meta type — the total-meta replacement for the deleted `metaOf` intrinsic. Its concrete params (`condition:
  *     Bool`) are pass-throughs the brace never projects, so their meta type is irrelevant and they stay verbatim (no
  *     `Bool$Meta` needed — a slotless type has meta `Unit`, which the channel supplies).
  *
  * The body is the meta *value* constructor `T$Meta(...)` applied to the brace's slot expressions (monomorphic), or the
  * single brace expression directly (generic — `fold`'s `join(whenTrue, whenFalse)`, whose `Meta.join` already returns
  * the arm meta type). Companions are compiler-pool-only (the channel evaluates them), dead in the runtime pool.
  */
object MetaTransferDesugarer {

  /** The `^Meta` transfer companion generated from `definition`'s return brace, or empty when it has none (every
    * ordinary def) or its signature shape is unsupported.
    */
  def desugar(definition: FunctionDefinition): Seq[(FunctionDefinition, RoleHint)] =
    if (definition.returnMeta.isEmpty) Seq.empty
    else transferCompanion(definition).map(_ -> RoleHint.NoHint).toSeq

  private def transferCompanion(f: FunctionDefinition): Option[FunctionDefinition] = {
    val generics = f.genericParameters.map(_.name.value).toSet
    // A generic companion keeps its original signature (its type params are reduced at their meta types by the channel,
    // its concrete params are unprojected pass-throughs); a monomorphic vessel suffixes each concrete type to `T$Meta`.
    for {
      metaArgs   <- if (generics.nonEmpty) f.args.some
                    else f.args.traverse(arg => metaTypeRef(arg.typeExpression).map(t => arg.copy(typeExpression = t)))
      metaReturn <- if (generics.nonEmpty) f.typeDefinition.some else metaTypeRef(f.typeDefinition)
      body       <- metaBody(f.typeDefinition, f.returnMeta, generics)
    } yield FunctionDefinition(
      f.name.map(qn => QualifiedName(qn.name, Qualifier.Meta)),
      f.genericParameters,
      metaArgs,
      metaReturn,
      Some(body),
      visibility = f.visibility
    )
  }

  /** The meta structure of a **concrete** value type expression: its application head `T` suffixed to `T$Meta` (the
    * suffix [[MetaConstructorDesugarer.metaTypeSuffix]]). Used only on a monomorphic vessel's signature, so the head is
    * always a slotted concrete type whose `T$Meta` resolves. `None` for a non-application head (unsupported).
    */
  private def metaTypeRef(typeExpr: Sourced[SourceExpression]): Option[Sourced[SourceExpression]] =
    typeExpr.value match {
      case SourceExpression.FunctionApplication(module, fnName, _, _) =>
        Some(typeExpr.as(SourceExpression.FunctionApplication(module, fnName.map(_ + MetaConstructorDesugarer.metaTypeSuffix), None, Seq.empty)))
      case _                                                          => None
    }

  /** The companion's body. When the return type is **concrete**, the meta value is the meta *constructor* applied to the
    * brace's slot expressions (`<ReturnHead>$Meta(<slots>)`, slot-producing). When the return type is a bare **type
    * parameter** there is no concrete constructor to wrap in, so the (single) brace expression *is* the result meta
    * structure directly (structure-producing — e.g. `fold`'s `join(whenTrue, whenFalse)`, whose `Meta.join` already
    * returns the arm meta type).
    */
  private def metaBody(
      returnType: Sourced[SourceExpression],
      braceExprs: Seq[Sourced[SourceExpression]],
      generics: Set[String]
  ): Option[Sourced[SourceExpression]] =
    returnType.value match {
      case SourceExpression.FunctionApplication(None, fnName, _, args) if args.isEmpty && generics.contains(fnName.value) =>
        braceExprs match {
          case Seq(single) => Some(single)
          case _           => None
        }
      case SourceExpression.FunctionApplication(_, fnName, _, _)                                                          =>
        Some(returnType.as(SourceExpression.FunctionApplication(None, fnName.map(_ + MetaConstructorDesugarer.metaTypeSuffix), None, braceExprs)))
      case _                                                                                                              => None
    }
}
