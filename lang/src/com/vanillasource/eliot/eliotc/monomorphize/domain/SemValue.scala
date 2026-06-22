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

  /** A stuck primitive (native) application — a built-in operation (`add`, `min`, `&&`, `fold`, `inc`, …) applied to
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

  /** Head of a stuck neutral term. */
  sealed trait NeutralHead
  object NeutralHead {
    case class VVar(level: Int, name: String) extends NeutralHead
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
