package com.vanillasource.eliot.eliotc.monomorphize.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.monomorphize.fact.ContributedBinding
import com.vanillasource.eliot.eliotc.platform.Platform
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*

/** The user supplier: contributes the [[ContributedBinding]] under [[ContributedBinding.userLabel]] for the binding the
  * NbE checker would build from a value's own (checking) body. A body-ful value contributes a
  * [[com.vanillasource.eliot.eliotc.monomorphize.fact.BindingContribution.Body]] (the merger closes it over its
  * dependencies on demand); a body-less value contributes `None` (its checking implementation is a native — or, for a
  * runtime-only function, the evaluator's stuck `VNeutral` fallback — never an empty user binding that would shadow a
  * reducing native, the `add` bug).
  *
  * Selected by the [[BindingMergerProcessor]] only when no native supplies the name; native+user coexistence (e.g.
  * `add`'s compile-time native and its runtime body) is benign, the native winning by category precedence.
  */
class UserValueNativesProcessor extends BodyContributorProcessor(ContributedBinding.userLabel, Platform.Runtime) {

  // Only this label's queries belong to the user contributor; short-circuit other labels before fetching SaturatedValue.
  override protected def generateFact(key: ContributedBinding.Key): CompilerIO[Unit] =
    if (key.label === ContributedBinding.userLabel) super.generateFact(key) else abort
}
