package com.vanillasource.eliot.eliotc.monomorphize.domain

import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.source.content.Sourced

/** The role a metavariable plays in the checker. One role per metavariable id, keyed in a single
  * `Map[Int, MetaRole]` on the [[com.vanillasource.eliot.eliotc.monomorphize.unify.Unifier]] (D2). This consolidates
  * what used to be six parallel side-tables spread across the unifier and `CheckState`: `combinable` / `candidates`
  * (unifier) and `combineResolved` / `pendingUpperBounds` / `carrierKinds` / `abstractTypeMetas` (`CheckState`).
  *
  * The payoff is that the post-check finalizer
  * ([[com.vanillasource.eliot.eliotc.monomorphize.check.TypeStackLoop.defaultUnsolvedMetas]]) becomes a **total match
  * on roles** with no catch-all: an unsolved [[AbstractAssoc]] stays unsolved, everything else defaults to `VType`.
  * A future role added here forces a decision at that match (non-exhaustiveness is reported) rather than silently
  * inheriting the old "default everything to `Type`" behaviour — the structural cure for the F2 fragility (a side-car
  * that forgets to resolve its meta would otherwise produce a silently mistyped value).
  *
  * A metavariable absent from the role map is [[Plain]] — every fresh meta starts there, and only the checker's
  * deliberate classifications ([[Instantiation]] on polytype peeling, [[AbstractAssoc]] on an abstract-ability-type
  * reference) move it off `Plain`.
  */
sealed trait MetaRole

object MetaRole {

  /** A metavariable with no special handling: solved by ordinary unification, or defaulted to [[SemValue.VType]] if
    * still unsolved at finalization. The default for any meta not otherwise classified (absence from the role map ⇒
    * `Plain`), e.g. the freshly-allocated domain/codomain metas of [[SemValue.VPi]] inference, the calculated-return
    * meta, and the `Coerce`-resolution scratch metas.
    */
  case object Plain extends MetaRole

  /** An implicit type-parameter *instantiation* metavariable — one peeled from a polytype's leading binders with a
    * fresh meta ([[com.vanillasource.eliot.eliotc.monomorphize.check.Checker.peelLams]]). Subsumes the architecture
    * review's separate `Combinable` and `Carrier` roles: a carrier is an instantiation meta, so the two are one role
    * with the carrier kind an optional attribute. (The `Combine`-based multi-candidate branch-join fields this role
    * once carried were removed with the refinement channel's flag day — `Int == Int` makes the join a no-op; see
    * `docs/bounds-as-refinements.md` Step 7b.)
    *
    * @param carrierKind
    *   When this meta stands for a *higher-kinded* type parameter (a `[F[_]]` carrier, kind `Type -> Type` etc.), its
    *   expected kind plus a call-site context for an error. Verified post-drain: a carrier solved to a value of the
    *   wrong kind is rejected rather than silently accepted.
    * @param effectCarrier
    *   Whether this meta stands for an *effect* carrier: a higher-kinded binder that is ability-constrained — either
    *   explicitly (an entry in the callee's `paramConstraints`, the M1 `{E...}` sugar's `[F[_] ~ E...]`) or implicitly
    *   by the callee's owning ability (`printLine`'s `F`, prepended from `ability Console[F[_]]`). A bare unconstrained
    *   HKT binder (`C[_, _]`) is *not* an effect carrier. Read by the checker-side effect lift to decide whether a
    *   `C[T']`-headed argument may be bind-lifted.
    */
  case class Instantiation(
      carrierKind: Option[(SemValue, Sourced[String])] = None,
      effectCarrier: Boolean = false
  ) extends MetaRole

  /** A standing placeholder for an abstract associated-ability-type (`type X` inside `ability ...`, no body). Solved
    * post-drain by unifying against the concrete impl's associated-type value once the ability resolves; if it never
    * resolves it must remain *unsolved* through quoting (so a constraint-covered reference stays abstract), which is
    * why it is the one role the finalizer protects from defaulting to `Type`.
    *
    * @param fqn
    *   The abstract type's fully-qualified name; its `Ability(...)` qualifier drives associated-type injection on
    *   ability resolution.
    */
  case class AbstractAssoc(fqn: ValueFQN) extends MetaRole
}
