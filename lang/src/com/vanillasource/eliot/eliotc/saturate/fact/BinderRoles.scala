package com.vanillasource.eliot.eliotc.saturate.fact

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedExpression
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedExpression.SignatureView
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedValue
import com.vanillasource.eliot.eliotc.source.content.Sourced

/** The runtime role of each leading type-stack ("generic") binder of a value, classified once on the *saturated*
  * signature + body (D6 of the monomorphize architecture review).
  *
  * The only role currently distinguished is whether a binder is '''reified''' — referenced in *value* position in the
  * runtime body, the positions [[com.vanillasource.eliot.eliotc.monomorphize.eval.Evaluator]] visits. A reified binder
  * is a binder of the value's *signature* but, crucially, '''not''' of its runtime body: `def bigOf[V] = V` has
  * signature `(V) -> BigInteger` yet body just `V` (the bare reference is the reification of the erased `V`). The
  * monomorphize binding cache must therefore wrap such a body in leading lambda binders so that applying the value's
  * explicit type arguments substitutes the reified binders (see `BindingProcessor.reifyingWrap`); an ordinary generic
  * whose parameter never appears in value position (`id[A](x: A) = x`) is left unwrapped.
  *
  * This is the single home for that classification — formerly the inline `valuePositionRefs` walk buried in
  * `BindingProcessor`. It is computed on the *saturated* signature (so the binder indices line up with the type
  * arguments the checker applies — `SaturatedValueProcessor` may prepend leading binders for omittable `auto`
  * parameters), and is the per-binder analysis the monomorphization-keying plan's B1 codegen-dedup is intended to share
  * (carried on [[SaturatedValue]], read both by checking and, eventually, by the codegen demand sites).
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

  /** One leading type-stack binder's runtime role.
    *
    * @param name
    *   The binder's name, with its source position (used to position the synthesized wrap lambda).
    * @param reified
    *   Whether the body references this binder in value position (and so the binding must materialise it at runtime).
    */
  case class Role(name: Sourced[String], reified: Boolean)

  /** Classify the leading type-stack binders of a value. The binders come from the value's signature
    * ([[SignatureView]]); a binder is reified iff its name appears in a value position of the value's [[runtime]] body.
    * The full `runtime` body is used (not `checkingRuntime`): for a non-`opaque` value they are identical, and for an
    * `opaque` value the checker's binding has no body to wrap, so the wrap never consults this — using `runtime` makes
    * the same analysis correct for the transparent (representation-lowering) binding too.
    */
  def of(value: OperatorResolvedValue): BinderRoles = {
    val signature = value.typeStack.as(value.typeStack.value.signature)
    val binders   = SignatureView.of(signature).binders
    val reified   = value.runtime.map(b => valuePositionRefs(b.value)).getOrElse(Set.empty)
    BinderRoles(binders.map(b => Role(b.name, reified.contains(b.name.value))))
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
}
