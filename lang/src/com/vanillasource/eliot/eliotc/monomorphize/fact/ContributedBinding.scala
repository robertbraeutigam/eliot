package com.vanillasource.eliot.eliotc.monomorphize.fact

import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.plugin.Configuration
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}

/** One supplier's *total* answer for how to reduce `vfqn` on the host during checking, under that supplier's `label`.
  *
  * Type-level evaluation is literally running code on the machine the compiler runs on, so for each name the checker
  * needs that name's host-runnable reduction. Several suppliers can offer one — the compiler/platform native reducers
  * (`Function`/`Type`/`Bool`, the `BigInteger` arithmetic, data-type constructors, `match` dispatch) and the per-layer
  * user-body supplier. Each emits this fact for *every* name it is asked about: a [[BindingContribution]] when it
  * defines a reduction/body for `vfqn`, `None` when it does not. A supplier states only what it knows about the name
  * *itself* — a [[BindingContribution.Leaf]] (a finished native reduction) or a [[BindingContribution.Body]] (its own
  * checking body); it never resolves dependencies, so it never reads back the [[NativeBinding]] it feeds. Distinct
  * `label`s keep the suppliers' answers in separate facts (one [[Key]] per `(vfqn, label)`), so they coexist with no
  * first-registration race; the [[com.vanillasource.eliot.eliotc.monomorphize.processor.BindingMergerProcessor]] then
  * selects one by category precedence (native before user), closes a selected `Body` over its dependencies, and
  * publishes the result as the single [[NativeBinding]] the checker reads.
  *
  * The fact is *total* — a supplier answers `None` rather than declining (aborting) — so the merger reads values with
  * `getFactOrAbort` and a mis-wired supplier fails loudly instead of silently demoting to a lower-precedence answer.
  * Absence is a value (`None`), not a missing fact.
  *
  * @param vfqn
  *   the value being asked about
  * @param label
  *   the supplier's label (see the `*Label` constants)
  * @param contributed
  *   the supplier's contribution for `vfqn`, or `None` if it does not define one
  */
case class ContributedBinding(
    vfqn: ValueFQN,
    label: String,
    contributed: Option[BindingContribution]
) extends CompilerFact {
  override def key(): CompilerFactKey[ContributedBinding] = ContributedBinding.Key(vfqn, label)
}

object ContributedBinding {
  case class Key(vfqn: ValueFQN, label: String) extends CompilerFactKey[ContributedBinding]

  /** The lang layer's compiler-intrinsic native reducers (`Function`/`Type`/`Bool` primitives). */
  val systemLabel: String = "system"

  /** Data-type constructors — inert, body-less `VTopDef` reductions. */
  val dataTypeLabel: String = "datatype"

  /** The `match`/`typeMatch` dispatch natives. */
  val matchLabel: String = "match"

  /** The compiler platform's Eliot-bodied reductions: the compile-time implementation a name has in the **compiler**
    * source pool (the `compiler` marker — base + the compiler-platform layer), read by
    * [[com.vanillasource.eliot.eliotc.monomorphize.processor.CompilerNativesProcessor]]. It is the compiler platform's
    * analogue of [[userLabel]] — the same value-body supplier — but reading the compiler pool and ranked as a *native*,
    * so its reduction wins for checking while the runtime layer's body is still used for codegen (the `add` pattern,
    * one name with two platform implementations). A name with no compiler-platform body contributes `None`.
    */
  val compilerLabel: String = "compiler"

  /** The single user supplier: a value's own (checking) body. The layer stack is merged to one implementation per name
    * upstream (`UnifiedModuleValueProcessor`), so one user contributor reading the unified body suffices.
    */
  val userLabel: String = "user"

  /** The native-category labels intrinsic to the language layer — always present in `LangProcessors`. Platform layers
    * (e.g. stdlib's compile-time arithmetic natives) register *additional* native labels through
    * [[extraNativeLabelsKey]]. [[compilerLabel]] is here too: the compiler platform is compiler-owned and always linked
    * (its layer is on the compiler path of every type-checking entry point), so its contributor is always in the
    * pipeline — when no compiler-platform layer is configured it simply contributes `None` everywhere.
    */
  val langNativeLabels: Seq[String] = Seq(systemLabel, dataTypeLabel, matchLabel, compilerLabel)

  /** The user-category labels. */
  val userLabels: Seq[String] = Seq(userLabel)

  /** Configuration roster of the *extra* native-category contributor labels added by non-base layers (stdlib, platform
    * plugins) in their `configure()`. The contributor set is open (layers are dynamic), so it cannot be a static list
    * of fact types; each contributing plugin adds its label(s) here, and the merger unions them with
    * [[langNativeLabels]]. The config carries only the label strings (*which* contributors exist), never per-FQN
    * semantics.
    */
  val extraNativeLabelsKey: Configuration.Key[Set[String]] = Configuration.namedKey("extraNativeBindingLabels")
}
