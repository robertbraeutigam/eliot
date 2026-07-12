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
  * The companion lives in the [[Qualifier.Meta]] namespace (so it never collides with `add`). Its parameter and return
  * types are the meta structures of the originals — obtained by a pure **name transform** (`Int` ⤳ `Int$Meta`, the
  * suffix [[MetaConstructorDesugarer.metaTypeSuffix]]), with **no cross-definition lookup**: a value of type `T` has
  * meta type `T$Meta` by construction (the meta structure `MetaConstructorDesugarer` emits from `type T {slots}`). The
  * body is the meta *value* constructor `T$Meta(...)` applied to the brace's slot expressions, so `a.range` resolves to
  * the meta structure's slot accessor and `a.range + b.range` is ordinary `Interval` arithmetic.
  *
  * A def whose signature is not a simple type application at every relevant position (e.g. a bare generic, an untracked
  * type with no meta structure) yields no companion — a documented limitation for now; the only defs carrying transfer
  * braces are the arithmetic natives over `Int`. Companions are compiler-pool-only (the channel evaluates them), dead
  * in the runtime pool, never code-generated.
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
    for {
      metaArgs   <- f.args.traverse(arg => metaTypeRef(arg.typeExpression, generics).map(t => arg.copy(typeExpression = t)))
      metaReturn <- metaTypeRef(f.typeDefinition, generics)
      body       <- metaBody(f.typeDefinition, f.returnMeta, generics)
    } yield FunctionDefinition(
      f.name.map(qn => QualifiedName(qn.name, Qualifier.Meta)),
      f.genericParameters, // generic parameters are KEPT: a generic transfer/merge companion (`fold^Meta[A]`) is
      metaArgs,            // instantiated per concrete `A`, and its meta types (`metaOf(A)`) reduce only then.
      metaReturn,
      Some(body),
      visibility = f.visibility
    )
  }

  /** The meta *type* reference for a value type expression. A **concrete** head is the plain name transform — the head's
    * name suffixed `$Meta`, bare (`Int[M1, X1]` ⤳ `Int$Meta`, since the meta structure is non-generic). A bare **type
    * parameter** `A` has no such structure (`A$Meta` is not a real type); its meta type is `metaOf(A)`, the type-level
    * intrinsic that reduces to `A`'s meta structure once `A` is concrete ([[WellKnownTypes.metaOfFQN]]). `None` for a
    * non-application head (unsupported).
    */
  private def metaTypeRef(typeExpr: Sourced[SourceExpression], generics: Set[String]): Option[Sourced[SourceExpression]] =
    typeExpr.value match {
      case SourceExpression.FunctionApplication(None, fnName, _, args) if args.isEmpty && generics.contains(fnName.value) =>
        // A type parameter A ⤳ metaOf(A): an unqualified `metaOf` (fold's source file imports `eliot.compiler.Meta`).
        Some(typeExpr.as(SourceExpression.FunctionApplication(None, fnName.as("metaOf"), None, Seq(typeExpr))))
      case SourceExpression.FunctionApplication(module, fnName, _, _)                                                     =>
        Some(typeExpr.as(SourceExpression.FunctionApplication(module, fnName.map(_ + MetaConstructorDesugarer.metaTypeSuffix), None, Seq.empty)))
      case _                                                                                                              => None
    }

  /** The companion's body. When the return type is **concrete**, the meta value is the meta *constructor* applied to the
    * brace's slot expressions (`<ReturnHead>$Meta(<slots>)`, slot-producing). When the return type is a bare **type
    * parameter** there is no concrete constructor to wrap in, so the (single) brace expression *is* the result meta
    * structure directly (structure-producing — e.g. `fold`'s `join(whenTrue, whenFalse)`, whose `Meta.join` already
    * returns `metaOf(A)`).
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
