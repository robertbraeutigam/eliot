package com.vanillasource.eliot.eliotc.monomorphize.fact

import com.vanillasource.eliot.eliotc.monomorphize.domain.SemValue
import com.vanillasource.eliot.eliotc.saturate.fact.SaturatedValue

/** What one supplier contributes for a name in a [[ContributedBinding]], *before* dependency closure. Splitting the two
  * shapes is what keeps the body suppliers out of the [[NativeBinding]] cycle: a supplier states only what it knows
  * about the name itself, and the [[com.vanillasource.eliot.eliotc.monomorphize.processor.BindingMergerProcessor]]
  * (the single owner of [[NativeBinding]]) does the recursive dependency walk for the *selected* contribution.
  */
enum BindingContribution {

  /** A finished, dependency-free reduction: the native leaves (the `Function`/`Type`/`Bool` primitives, the
    * `BigInteger` arithmetic, the inert data-type constructors, the `match` dispatch). Nothing more to resolve — the
    * merger publishes it as the [[NativeBinding]] directly.
    */
  case Leaf(value: SemValue)

  /** A body supplier's own (checking) body, carried as the value's [[SaturatedValue]] so the merger can close it over
    * its dependencies. Only the value's *own* body is named here; the dependency lookup against [[NativeBinding]] is
    * done by the merger, never by the supplier — that is what breaks the former merger ⇄ supplier cycle.
    *
    * Emitted only for a body-ful value; a body-less value is declined with `contributed = None` (its checking
    * implementation is a native, or the evaluator's stuck `VNeutral` fallback). An `opaque` definition is body-ful here
    * but its checking body is empty, so the merger builds a stuck `VTopDef` from it — exactly like a body-less native.
    */
  case Body(saturated: SaturatedValue)
}
