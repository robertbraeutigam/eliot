package com.vanillasource.eliot.eliotc.monomorphize.eval

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.monomorphize.domain.SemValue
import com.vanillasource.eliot.eliotc.monomorphize.domain.SemValue.*
import com.vanillasource.eliot.eliotc.monomorphize.domain.MetaStore
import com.vanillasource.eliot.eliotc.monomorphize.fact.GroundValue

/** Quotes semantic values back to ground values. This is the "read-back" phase of NbE.
  *
  * Fails on VNeutral, VMeta, VLam — those should be resolved before quoting.
  */
object Quoter {

  /** Quote a semantic value to a ground value, forcing metas as needed. */
  def quote(depth: Int, v: SemValue, metaStore: MetaStore): Either[String, GroundValue] = {
    val forced = Evaluator.force(v, metaStore)
    forced match {
      case VType =>
        Right(GroundValue.Type)

      case VConst(ground) =>
        Right(ground)

      case VPi(domain, codomain) =>
        for {
          domGround <- quote(depth, domain, metaStore)
          codGround <- quote(depth + 1, codomain(freshVar(depth)), metaStore)
        } yield GroundValue.Structure(
          Map(
            "$typeName" -> GroundValue.Direct(
              com.vanillasource.eliot.eliotc.eval.fact.Types.functionDataTypeFQN,
              GroundValue.Type
            ),
            "A"         -> domGround,
            "B"         -> codGround
          ),
          GroundValue.Type
        )

      case VNeutral(_, _, _) =>
        Left("Cannot quote neutral value — contains unresolved variable")

      case VMeta(id, _, _) =>
        Left(s"Cannot quote unsolved meta ?${id.value}")

      case VLam(_, _) =>
        Left("Cannot quote lambda — expected a fully evaluated type")

      case VNative(_, _) =>
        Left("Cannot quote partially applied native")

      case VTopDef(fqn, _, _) =>
        Left(s"Cannot quote unapplied top-level definition ${fqn.show}")
    }
  }

  private def freshVar(depth: Int): SemValue =
    VNeutral(NeutralHead.VVar(depth, s"$$quote$depth"), Spine.SNil, VType)
}
