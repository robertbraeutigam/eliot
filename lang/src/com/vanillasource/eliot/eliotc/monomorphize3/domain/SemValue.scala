package com.vanillasource.eliot.eliotc.monomorphize3.domain

import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.monomorphize3.fact.GroundValue

/** The semantic domain for Normalisation by Evaluation (NbE). ORE syntax is evaluated into SemValues via closures. Type
  * equality becomes structural equality of normal forms.
  *
  * - VType: the type of all types.
  * - VConst: non-function ground values (concrete types, literals).
  * - VLam: runtime lambda closure. Produced by the evaluator for FunctionLiteral.
  * - VPi: function type (dependent or non-dependent). Produced by the Checker or Function native.
  * - VNative: primitive that fires on concrete arguments.
  * - VTopDef: lazy top-level definition with a spine.
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

  case class VMeta(id: MetaId, spine: Spine, expected: SemValue) extends SemValue

  case class VNeutral(head: NeutralHead, spine: Spine, tpe: SemValue) extends SemValue

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
