package com.vanillasource.eliot.eliotc.saturate.fact

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedExpression
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedExpression.SignatureView
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedValue
import com.vanillasource.eliot.eliotc.source.content.Sourced

/** The codegen-relevance classification of each leading type-stack ("generic") binder of a value, computed once,
  * statically (no evaluation), on the *saturated* signature + body. This is the monomorphization-keying plan's **B1**
  * analysis; it grows out of the D6 reified-binder analysis (originally only [[Role.reified]], whose single consumer was
  * the monomorphize binding wrap).
  *
  * Each binder gets three orthogonal role flags and one derived [[BinderRoles.Disposition]] that drives the
  * codegen-key projection ([[com.vanillasource.eliot.eliotc.used.CodegenProjection]]):
  *
  *   - '''reified''' (R1) — referenced in *value* position in the runtime body, the positions
  *     [[com.vanillasource.eliot.eliotc.monomorphize.eval.Evaluator]] visits. A reified binder is a binder of the
  *     value's *signature* but not of its runtime body (`def bigOf[V] = V` has body just `V`), so the binding cache must
  *     materialise it (see `BindingClosure.reifyingWrap`).
  *   - '''dispatched''' (R2) — drives an ability-instance selection: the binder is constrained (`[A ~ Show]`, so it is a
  *     key of `paramConstraints`) or appears in a constraint's type arguments. Dispatch must never be merged
  *     (soundness), so it forces [[BinderRoles.Disposition.Specialize]].
  *   - '''representation''' (R3) — appears in a value-parameter type or the return type, so it shapes the machine
  *     representation of runtime data (`id[A](x: A): A` ⟹ `A` is what `RepresentationLowering.representationOf`
  *     consumes). Maximally conservative: *any* appearance in a runtime-data type counts, even a size index that does
  *     not really reach representation (`List[A, N]`'s `N`) — refining that into a true phantom is a later tightening.
  *
  * Disposition (the per-binder codegen-key decision; see the plan's table). Precedence is soundness-ordered — dispatch
  * is checked first because merging distinct dispatch is a miscompile:
  *
  * {{{
  *   dispatched     -> Specialize               (never merge an ability selection)
  *   reified        -> Specialize               (a bounded reified family stays distinct)
  *   representation -> CollapseToRepresentation (key on the width class, not the exact bound)
  *   otherwise      -> CollapseErase            (an obvious phantom: appears in no scanned position)
  * }}}
  *
  * The classification is computed on the *saturated* signature (so binder indices line up with the type arguments the
  * checker applies — `SaturatedValueProcessor` may prepend leading binders for omittable `auto` parameters) and is
  * carried on [[SaturatedValue]], its two intended consumers being type-checking's binding wrap and the codegen dedup.
  *
  * The projection only ever folds together instances whose generated code is identical, so it is a pure code-size
  * optimization and never affects correctness. Because Eliot user code cannot express recursion (see the "Total by
  * Default" cornerstone in `.claude/CLAUDE.md`), the number of distinct instantiations is finite and program-shaped — there is no
  * unbounded family to collapse, so the earlier recursion-variance / demote machinery has been removed; the `used`
  * non-convergence backstop remains as the fail-safe for type-level divergence.
  *
  * @param roles
  *   One [[BinderRoles.Role]] per leading type-stack binder, in declaration order.
  */
case class BinderRoles(roles: Seq[BinderRoles.Role]) {

  /** The leading binders up to and including the last reified one — the contiguous prefix the binding wrap covers. A
    * non-reified binder inside that prefix is still wrapped (as an unused binder, harmless because its type argument is
    * threaded anyway) so that explicit-type-argument application stays positionally aligned. Empty when nothing is
    * reified (the body is then left unwrapped).
    */
  def reifiedPrefixBinders: Seq[Sourced[String]] = {
    val lastIndex = roles.lastIndexWhere(_.reified)
    if (lastIndex < 0) Seq.empty else roles.take(lastIndex + 1).map(_.name)
  }
}

object BinderRoles {

  /** The codegen-key disposition of one binder — how the codegen projection treats this position when building the
    * codegen dedup key. See the table in [[BinderRoles]].
    */
  enum Disposition {

    /** Phantom: appears in no scanned position. Dropped from the codegen key, with no runtime form. */
    case CollapseErase

    /** Representation-determining: key on `RepresentationLowering.representationOf` (the width class), so
      * width-equivalent bounds fold to one specialization while the nominal head is preserved for dispatch.
      */
    case CollapseToRepresentation

    /** Kept verbatim in the codegen key — a distinct ability selection (R2) or a bounded reified family (R1), each a
      * genuinely different body.
      */
    case Specialize
  }

  /** One leading type-stack binder's runtime/codegen role.
    *
    * @param name
    *   The binder's name, with its source position (used to position the synthesized wrap lambda).
    * @param reified
    *   R1: the body references this binder in value position (so the binding must materialise it at runtime).
    * @param dispatched
    *   R2: this binder drives an ability-instance selection (it is constrained, or appears in a constraint's args).
    * @param representation
    *   R3: this binder appears in a value-parameter type or the return type, so it shapes runtime-data representation.
    */
  case class Role(
      name: Sourced[String],
      reified: Boolean,
      dispatched: Boolean,
      representation: Boolean
  ) {

    /** The codegen-key disposition derived from the role flags (see [[BinderRoles]] for the precedence rationale). */
    def disposition: Disposition =
      if (dispatched) Disposition.Specialize
      else if (reified) Disposition.Specialize
      else if (representation) Disposition.CollapseToRepresentation
      else Disposition.CollapseErase
  }

  /** Classify the leading type-stack binders of a value. The binders come from the value's signature
    * ([[SignatureView]]); each flag is derived from a different facet of the value:
    *
    *   - [[Role.reified]] from value-position references in the [[OperatorResolvedValue.runtime]] body;
    *   - [[Role.dispatched]] from [[OperatorResolvedValue.paramConstraints]];
    *   - [[Role.representation]] from references in the signature's value-parameter domains and return position.
    *
    * The full `runtime` body is used (not `checkingRuntime`): for a non-`opaque` value they are identical, and for an
    * `opaque` value the checker's binding has no body to wrap, so the wrap never consults this; using `runtime` keeps
    * the same analysis correct for the transparent (representation-lowering) binding too. The whole computation reads
    * only the value's own fields — no fact lookups — so it stays pure and local.
    */
  def of(value: OperatorResolvedValue): BinderRoles = {
    val signature = value.typeStack.as(value.typeStack.value.signature)
    val view      = SignatureView.of(signature)
    val binders   = view.binders
    val body      = value.runtime.map(_.value)

    val reified        = body.map(valuePositionRefs).getOrElse(Set.empty)
    val dispatched     = dispatchedRefs(value.paramConstraints)
    val representation = (view.parameters.map(_.value) :+ view.returnType.value).flatMap(parameterRefs).toSet

    BinderRoles(binders.map { b =>
      val name = b.name.value
      Role(
        b.name,
        reified = reified.contains(name),
        dispatched = dispatched.contains(name),
        representation = representation.contains(name)
      )
    })
  }

  /** The set of parameter names referenced in positions [[com.vanillasource.eliot.eliotc.monomorphize.eval.Evaluator]]
    * actually visits: value-position references, and the type arguments of nested value references (which `eval`
    * evaluates into the spine). A [[OperatorResolvedExpression.FunctionLiteral]]'s parameter-type annotation is
    * deliberately '''not''' walked — `eval` ignores it — and the literal's own bound name is excluded so a shadowing
    * value parameter is not mistaken for a reified type-stack binder.
    */
  private def valuePositionRefs(expr: OperatorResolvedExpression): Set[String] = expr match {
    case OperatorResolvedExpression.ParameterReference(name)    => Set(name.value)
    case OperatorResolvedExpression.ValueReference(_, typeArgs) =>
      typeArgs.flatMap(ta => valuePositionRefs(ta.value)).toSet
    case OperatorResolvedExpression.FunctionApplication(t, a)   =>
      valuePositionRefs(t.value) ++ valuePositionRefs(a.value)
    case OperatorResolvedExpression.FunctionLiteral(pn, _, b)   => valuePositionRefs(b.value) - pn.value
    case _                                                      => Set.empty
  }

  /** The binder names that drive ability dispatch: every constrained binder (a key of `paramConstraints`) plus every
    * binder referenced inside a constraint's type arguments (so a binder participating in selecting the instance —
    * `[A, B ~ Foo[A]]` — is kept too). Conservative on purpose: marking a binder dispatched only ever *prevents* a
    * collapse, never causes an unsound merge.
    */
  private def dispatchedRefs(
      paramConstraints: Map[String, Seq[OperatorResolvedValue.ResolvedAbilityConstraint]]
  ): Set[String] =
    paramConstraints.keySet ++
      paramConstraints.values.flatten.flatMap(_.typeArgs.flatMap(parameterRefs)).toSet

  /** Every free [[OperatorResolvedExpression.ParameterReference]] name occurring anywhere in a *type* expression —
    * including type-constructor arguments and (unlike [[valuePositionRefs]]) `FunctionLiteral` parameter-type
    * annotations, since a type-level lambda's domain is a real type position. A literal's own bound name is excluded
    * (shadowing). Used to find which binders shape a value-parameter / return type (R3) or a constraint's arguments.
    */
  private def parameterRefs(expr: OperatorResolvedExpression): Set[String] = expr match {
    case OperatorResolvedExpression.ParameterReference(name)    => Set(name.value)
    case OperatorResolvedExpression.ValueReference(_, typeArgs) =>
      typeArgs.flatMap(ta => parameterRefs(ta.value)).toSet
    case OperatorResolvedExpression.FunctionApplication(t, a)   =>
      parameterRefs(t.value) ++ parameterRefs(a.value)
    case OperatorResolvedExpression.FunctionLiteral(pn, pt, b)  =>
      val inType = pt.toSeq.flatMap(_.value.levels.toList).flatMap(parameterRefs).toSet
      inType ++ (parameterRefs(b.value) - pn.value)
    case _                                                      => Set.empty
  }
}
