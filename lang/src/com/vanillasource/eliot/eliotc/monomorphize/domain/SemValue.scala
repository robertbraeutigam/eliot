package com.vanillasource.eliot.eliotc.monomorphize.domain

import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.monomorphize.fact.GroundValue

/** The semantic domain for Normalisation by Evaluation (NbE). ORE syntax is evaluated into SemValues via closures. Type
  * equality becomes structural equality of normal forms.
  *
  * - VType: the type of all types.
  * - VConst: non-function ground values (concrete types, literals).
  * - VLam: runtime lambda closure. Produced by the evaluator for FunctionLiteral.
  * - VPi: function type (dependent or non-dependent). Produced by the Checker or Function native.
  * - VNative: primitive that fires on concrete arguments.
  * - VTopDef: lazy top-level definition with a spine.
  * - VStuckNative: a primitive (native) application stuck on not-yet-concrete arguments.
  * - VMeta: unsolved metavariable with a spine and expected type.
  * - VNeutral: stuck application with a variable head and a spine.
  */
sealed trait SemValue

object SemValue {
  case object VType extends SemValue

  case class VConst(ground: GroundValue) extends SemValue

  case class VLam(name: String, closure: SemValue => SemValue) extends SemValue

  case class VPi(domain: SemValue, codomain: SemValue => SemValue) extends SemValue

  case class VNative(paramType: SemValue, fire: SemValue => SemValue) extends SemValue

  case class VTopDef(fqn: ValueFQN, cached: Option[Lazy[SemValue]], spine: Spine) extends SemValue

  /** A stuck primitive (native) application — a built-in operation (`add`, `min`, `&&`, `fold`, …) applied to
    * arguments that are not yet concrete, so it cannot reduce. Carries the native's own FQN and the (renormalised)
    * argument spine.
    *
    * Kept **distinct from [[VTopDef]]** on purpose (D3): a native application is **not injective** (`add(1, 3)` and
    * `add(2, 2)` are equal), so it must never be injectivity-decomposed by the unifier the way a type/data constructor
    * is, and the quoter must fail loudly rather than read it back as a ground `Structure` type if one ever survives to
    * read-back. [[com.vanillasource.eliot.eliotc.monomorphize.eval.Evaluator.renormalize]] re-fires it once its
    * arguments become concrete; two such applications are definitionally equal only when their FQNs and spines match.
    */
  case class VStuckNative(fqn: ValueFQN, spine: Spine) extends SemValue

  case class VMeta(id: MetaId, spine: Spine) extends SemValue

  case class VNeutral(head: NeutralHead, spine: Spine) extends SemValue

  /** Opaque wrapper for metavariable identifiers. */
  opaque type MetaId = Int
  object MetaId {
    def apply(id: Int): MetaId          = id
    extension (id: MetaId) def value: Int = id
  }

  /** Head of a stuck neutral term. Identity is **structural** — the constructor and its fields — so neutrals minted by
    * different subsystems (parameter binders, the unifier's and quoter's binder-descending probes, the reserved
    * markers) can never collide even at a coinciding numeric level/depth, without relying on reserved name strings or
    * magic level numbers (the F3 hardening).
    */
  sealed trait NeutralHead {

    /** A human-readable name for diagnostics (error messages, `SemValuePrinter`). Never an identity — heads compare by
      * constructor.
      */
    def name: String
  }
  object NeutralHead {

    /** A genuine bound variable — a runtime value parameter, an unresolved parameter reference during evaluation, a
      * read-back binder — and the throwaway named placeholders diagnostics/printing substitute under a binder. Identity
      * is `(level, name)`.
      */
    case class Param(level: Int, name: String) extends NeutralHead

    /** A *signature-twin generic binder* left unapplied at a partial-arity key (signature-unification C2): distinct from
      * [[Param]] (a runtime value parameter) precisely because it read-backs differently — a `Param` neutral surviving to
      * read-back is a legitimately-structural runtime parameter that keeps a body structural, whereas a [[SignatureBinder]]
      * neutral quotes to a [[com.vanillasource.eliot.eliotc.monomorphize.fact.GroundValue.Param]] (a parametric signature).
      * `index` is the binder's position; `name` its declared name (for diagnostics). Bound in ρ by `bindTwinBinders` for a
      * binder the key carries no argument for.
      */
    case class SignatureBinder(index: Int, name: String) extends NeutralHead

    /** A fresh rigid variable a binder-descending traversal injects to compare or probe under a [[VPi]]/[[VLam]]: the
      * unifier (identity-bearing — both codomains receive the same one) and the quoter (a throwaway probe). Keyed by
      * [[Origin]] and the traversal's local `depth`, so it can never coincide with a [[Param]] or the other traversal's
      * var even at an equal numeric depth — replacing the former `$unify<n>` / `$quote<n>` reserved-name convention.
      */
    case class Fresh(origin: Origin, depth: Int) extends NeutralHead {
      override def name: String = s"$$${origin.tag}$depth"
    }

    /** A reserved, binder-less marker with no scope: the fail-safe bad-apply head, the effectful-guard signature probe,
      * the stuck-match placeholder. Identity is the [[Marker]] — recognised by constructor, not by a magic name string.
      */
    case class Reserved(marker: Marker) extends NeutralHead {
      override def name: String = marker.tag
    }

    /** Which binder-descending traversal minted a [[Fresh]] var. */
    enum Origin(val tag: String) {
      case Unify extends Origin("unify")
      case Quote extends Origin("quote")
    }

    /** Which reserved, scope-less marker a [[Reserved]] head stands for. The `tag`s preserve the former display names. */
    enum Marker(val tag: String) {
      case BadApply   extends Marker("$bad-apply")
      case GuardProbe extends Marker("$guard-probe")
      case Match      extends Marker("match")
    }
  }

  /** Spine of applied arguments, reversed cons list for O(1) append. */
  sealed trait Spine {
    def :+(arg: SemValue): Spine = Spine.SApp(this, arg)

    def toList: List[SemValue] = {
      @scala.annotation.tailrec
      def go(spine: Spine, acc: List[SemValue]): List[SemValue] = spine match {
        case Spine.SNil          => acc
        case Spine.SApp(tl, hd) => go(tl, hd :: acc)
      }
      go(this, Nil)
    }
  }
  object Spine {
    case object SNil                               extends Spine
    case class SApp(tail: Spine, head: SemValue) extends Spine
  }

  /** A lazy value that is evaluated at most once. */
  class Lazy[A](computation: () => A) {
    lazy val value: A = computation()
  }
  object Lazy {
    def apply[A](computation: => A): Lazy[A] = new Lazy(() => computation)
  }
}
