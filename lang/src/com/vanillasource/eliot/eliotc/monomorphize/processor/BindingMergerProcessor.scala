package com.vanillasource.eliot.eliotc.monomorphize.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.monomorphize.domain.SemValue
import com.vanillasource.eliot.eliotc.monomorphize.fact.{BindingContribution, ContributedBinding, NativeBinding}
import com.vanillasource.eliot.eliotc.platform.Platform
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.SingleFactProcessor

/** Owns the evaluator-facing [[NativeBinding]]: for each name it selects the one host-runnable reduction by category
  * precedence over the suppliers' total [[ContributedBinding]] facts, then closes the selected body over its
  * dependencies. This is the single owner that replaces the former silent first-registration race between the several
  * `NativeBinding` producers — and the single owner of the [[NativeBinding]] recursion (see below).
  *
  * Type-level evaluation is *running code on the host*, so for each name the checker needs that name's host-runnable
  * reduction. Two categories of supplier can offer one:
  *   - **native suppliers** (`nativeLabels`) — compiler/platform reducers coded directly against the evaluator;
  *     host-runnable by construction and disjoint (each owns its own names). **Preferred for checking.**
  *   - **user suppliers** (`userLabels`) — the per-layer body suppliers, merged upstream to one implementation per name
  *     by the layering system. **Fallback**, used when no native supplies the name; non-runnability surfaces lazily at
  *     the use site, never as a producer-side judgment.
  *
  * Selection is pure precedence with no conflict resolution: the first native `Some` → else the first user `Some` →
  * else abort (no binding; the evaluator stalls at the use site — the companion-clause, use-site error). Uniqueness
  * already holds within each category (native suppliers are disjoint by construction; the user stack is
  * single-implementation by layering), so each category yields at most one value and there is nothing to order or
  * reject. A user body coexisting with a native is the normal case (`add` has a compile-time native and a runtime
  * body), not an override — precedence alone keeps the native for checking; the body is read by codegen via
  * `TransparentBinding`.
  *
  * **Dependency closure lives here, not in the suppliers.** A supplier contributes only what it knows about the name
  * *itself*: a [[BindingContribution.Leaf]] (a finished native reduction) or a [[BindingContribution.Body]] (its own
  * checking body, carried as a `SaturatedValue`). For the selected `Body`, this processor runs [[BindingClosure]],
  * which walks the body and resolves each dependency via [[NativeBinding]] — i.e. recursively through *this* processor.
  * Keeping that walk here means the body suppliers never read back the merged fact they feed: the only cycle in the
  * graph is [[NativeBinding]]'s own self-recursion (a value's binding referencing its dependencies' bindings), owned by
  * this single processor and guarded by the active fact-request chain inside [[BindingClosure]] — not a merger ⇄
  * supplier cycle.
  *
  * Contributor facts are total, so [[getFactOrAbort]] always lands and a mis-wired native supplier aborts loudly (no
  * binding ⇒ use-site stuck) rather than silently demoting to the user answer. The user contributors are only forced
  * when no native wins, so native-owned names never pay for the user contribution.
  */
class BindingMergerProcessor(nativeLabels: Seq[String], userLabels: Seq[String])
    extends SingleFactProcessor[NativeBinding.Key] {

  override def generateSingleFact(key: NativeBinding.Key): CompilerIO[NativeBinding] =
    for {
      natives  <- nativeLabels.traverse(label => getFactOrAbort(ContributedBinding.Key(key.vfqn, label)))
      selected <- natives.flatMap(_.contributed).headOption match {
                    case found @ Some(_) => found.pure[CompilerIO]
                    // The `user` suppliers read the *runtime* pool, so they belong to the runtime track only. The
                    // compiler track never consults them: a compiler-platform value with a body is supplied by the
                    // `compiler` native label (a compiler-pool body), and a name with no compiler reduction stays
                    // abstract rather than falling through to a runtime body — the native-leaf boundary.
                    case None            =>
                      key.platform match {
                        case Platform.Compiler => Option.empty[BindingContribution].pure[CompilerIO]
                        case Platform.Runtime  =>
                          userLabels
                            .traverse(label => getFactOrAbort(ContributedBinding.Key(key.vfqn, label)))
                            .map(_.flatMap(_.contributed).headOption)
                      }
                  }
      semValue <- selected match {
                    case Some(BindingContribution.Leaf(value))     => value.pure[CompilerIO]
                    case Some(BindingContribution.Body(saturated)) =>
                      BindingClosure.buildBinding(saturated, _.runtime, key.platform)
                    case None                                      => abort[SemValue]
                  }
    } yield NativeBinding(key.vfqn, semValue, key.platform)
}
