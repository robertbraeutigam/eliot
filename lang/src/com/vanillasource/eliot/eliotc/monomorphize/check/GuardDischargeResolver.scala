package com.vanillasource.eliot.eliotc.monomorphize.check

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.module.fact.WellKnownTypes
import com.vanillasource.eliot.eliotc.monomorphize.check.CheckIO.*
import com.vanillasource.eliot.eliotc.monomorphize.domain.*
import com.vanillasource.eliot.eliotc.monomorphize.domain.SemValue.*
import com.vanillasource.eliot.eliotc.monomorphize.fact.GroundValue
import com.vanillasource.eliot.eliotc.monomorphize.fact.GroundValue.Literal
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerError

/** The **effectful-signatures discharge** (W2b), factored out of the
  * [[com.vanillasource.eliot.eliotc.monomorphize.check.Checker]] into one cohesive module: a return-type expression may
  * be a `{Throw[String]}` computation on the compile-time `Either[String, _]` carrier, which the compiler is the
  * handler for. "Run a compile-time computation to obtain the return type": the guard `runThrow`s the signature's
  * `Either` and reads `Right(t)` (the type) or `Left(msg)` (a rejection).
  *
  * Three discharge hook points:
  *   - [[isGuardCarrier]] (the *kind* position — accept an `Either[..]`-valued return where a bare `Type` is expected),
  *   - [[dischargeGuardedSignature]] (the *callee* side — clean the published signature),
  *   - [[dischargeGuardedReturn]] (the *applied / by-name read* sides — discharge a guard a caller observes).
  *
  * Operates over [[CheckIO]], reading the shared [[CheckState]] through `get`. It depends on exactly two checker
  * primitives, passed at construction — that narrow surface is the module boundary.
  *
  * @param force
  *   Force a SemValue through the current meta store — the checker's `force`.
  * @param freshMeta
  *   Allocate a fresh metavariable — the checker's `freshMeta`.
  */
class GuardDischargeResolver(
    force: SemValue => CheckIO[SemValue],
    freshMeta: CheckIO[VMeta]
) {

  /** The guard-carrier recognition for the *kind* position (W2b, and ability-implementation guards). A return-type
    * expression whose *value* is a guard denotes a type/verdict but may instead reject, so its inferred *type* is not
    * the bare `Type` an ordinary return position has. Two guard-carrier shapes are accepted where a `Type` kind is
    * expected:
    *   - the effectful-signatures carrier `Either[String, _]` ([[WellKnownTypes.eitherFQN]]-headed), from an inline
    *     `if(cond, T) else raise(msg)` / bare `raise(msg)` guard on the compile-time `Throw[String]` channel — discharged
    *     to its payload type (or rejected) by [[dischargeGuardedSignature]] / [[dischargeGuardedReturn]] once concrete;
    *   - a bare `Bool` ([[WellKnownTypes.boolFQN]]-headed), the applicability verdict of an ability-implementation
    *     `where` guard riding the marker's return slot (ability-guards §2.3) — read as keep/decline at the use-site
    *     discharge in the ability processor.
    *
    * `true` here means "accept this as a guarded return", since the kind check would otherwise reject `Either[..]` /
    * `Bool` as ≠ `Type`. A *normal* `Bool` or `Either[..]` used as a *return type* infers kind `Type` (a fully-applied
    * type constructor `Either[String, Int]` is a type), so `inferred` is `VType` there and this is unaffected — only a
    * `Bool`/`Either`-*typed value* sitting in a type position (a `true` / `E1 != E2` / `raise("…")`) is a guard.
    */
  def isGuardCarrier(inferred: SemValue): CheckIO[Boolean] =
    force(inferred).flatMap {
      case VTopDef(fqn, _, _, _) => pure(fqn === WellKnownTypes.eitherFQN || fqn === WellKnownTypes.boolFQN)
      // An *inline* guard (`if..else..raise`) whose carrier is still an unsolved **higher-kinded** meta at kind-check
      // time (`?G[?A]`, the `else` carrier not yet pinned): its concrete carrier `Either[String]` is only fixed later by
      // `Track.Compiler.pinCarriers`, but the return already *is* a `{Throw[String]}`/`{Abort}` guard. Recognising the
      // higher-kinded head here accepts it as a guarded return, so the kind check never postpones `?G[?A] ~ Type` — a
      // constraint that would become a hard `Either[String, _] ~ Type` mismatch once the carrier is pinned. Only a
      // higher-kinded meta qualifies — an ordinary `[A]` binder's meta is not recorded — and a normal effectful return
      // (`{Console} Unit`) never reaches this ladder with a `Type` expectation, so it does not over-fire (verified
      // against the effect examples).
      case VMeta(id, _)       => inspect(_.unifier.isHigherKindedMeta(id.value))
      case _                  => pure(false)
    }

  /** Discharge a *single* return value on the compile-time `Throw[String]` carrier — the W2b handler. The return
    * computation has been forced to a ground `Either[String, Type]`:
    *   - `Right(t)` ⟹ `Some(t)`: the resolved return type is the payload `t`.
    *   - `Left(msg)` ⟹ `compilerError(at.as(msg))` then abort: the guard rejected, with the author's message primary.
    *   - anything else (an ordinary type, or a still-stuck guard whose bounds are abstract) ⟹ `None`: nothing to
    *     discharge / defer to the use site (a stuck guard is correct, not an error — Use-Site Verification).
    *
    * `Right`/`Left` are body-less value constructors, so a constructed carrier value is a `VTopDef` headed by
    * [[WellKnownTypes.rightFQN]]/[[WellKnownTypes.leftFQN]]; the payload spine entry and the rejection fallback are
    * the [[GuardChannel]] protocol shared with the ability-guard verdict interpreter.
    */
  def dischargeGuardedReturn(retType: SemValue, at: Sourced[?]): CheckIO[Option[SemValue]] =
    force(retType).flatMap {
      case VTopDef(fqn, _, spine, _) if fqn === WellKnownTypes.rightFQN =>
        pure(GuardChannel.payload(spine.toList))
      case VTopDef(fqn, _, spine, _) if fqn === WellKnownTypes.leftFQN  =>
        GuardChannel.payload(spine.toList) match {
          case Some(msgSem) =>
            extractGuardMessage(msgSem).flatMap(msg => liftF(compilerError(at.as(msg)) >> abort[Option[SemValue]]))
          case None         => pure(None)
        }
      case _                                                        => pure(None)
    }

  /** The author message carried by a `Left(msg)` rejection. The carrier's error type is `String`, so a rejection's
    * message is a literal `String` that reads back directly; a non-literal (computed, not-yet-reduced) message falls
    * back to the shared [[GuardChannel]] rejection message so the guard is still reported, never silently dropped
    * (fail-safe).
    */
  private def extractGuardMessage(msgSem: SemValue): CheckIO[String] =
    force(msgSem).map {
      case VConst(GroundValue.Direct(Literal.StringValue(s), _)) => s
      case _                                        => GuardChannel.fallbackRejectionMessage
    }

  /** Discharge any guard in a *signature's* return position — the callee side (W2b), settled at the read of the value's
    * (re-inflated, ground) signature twin (signature-unification C1). The signature is a chain of value-parameter `VPi`
    * arrows ending in the return; descend to that return — a guard depends only on the (now concrete) type parameters, so
    * a placeholder suffices to peel the arrows — and settle it **by its ground shape**, no `sawGuard` flag:
    *   - `Right(t)` ⟹ rebuild the arrows over the payload `t`, so the published signature and the body's expected type
    *     become the plain type `t`; no return meta.
    *   - `Left` ⟹ the discharge aborts (the author message).
    *   - an `Either`/`Bool` **carrier-headed** return ([[isGuardCarrier]]) — a guard the twin published *undischarged*
    *     (its verdict depends on a leftover generic binder the twin left as a `GroundValue.Param`) — with a body ⟹
    *     **defer**: a fresh return metavariable the body solves, so the body type-checks instead of erroring against the
    *     undischarged carrier, and the guard is re-decided at each concrete use (Use-Site Verification).
    *   - anything else (an ordinary type, or a body-less carrier-headed return) ⟹ returned untouched (the latter stays
    *     stuck and hard-errors at read-back — fail-safe).
    *
    * @return
    *   the resolved signature and an optional return metavariable (`Some` only in the deferred case, fed to the same
    *   post-drain fail-safe as an undetermined deferred return).
    */
  def dischargeGuardedSignature(
      sig: SemValue,
      hasBody: Boolean,
      at: Sourced[?]
  ): CheckIO[(SemValue, Option[VMeta])] = {
    // A guard's return depends only on the (now concrete) type parameters, so any value stands in to peel the
    // value-parameter arrows; a fresh neutral keeps the metastore clean (unlike a meta) for the non-guard common case.
    val probe: SemValue = VNeutral(NeutralHead.Reserved(NeutralHead.Marker.GuardProbe), Spine.SNil)

    def rebuild(domains: List[SemValue], leaf: SemValue): SemValue =
      domains.foldLeft(leaf)((acc, dom) => VPi(dom, _ => acc))

    def peel(current: SemValue, domains: List[SemValue]): CheckIO[(SemValue, Option[VMeta])] =
      force(current).flatMap {
        case VPi(domain, codomain) => peel(codomain(probe), domain :: domains)
        case leaf                  =>
          dischargeGuardedReturn(leaf, at).flatMap {
            case Some(payload) => pure((rebuild(domains, payload), None))
            case None          =>
              isGuardCarrier(leaf).flatMap {
                case true if hasBody => freshMeta.map(m => (rebuild(domains, m), Some(m)))
                case _               => pure((sig, None))
              }
          }
      }

    peel(sig, Nil)
  }

}
