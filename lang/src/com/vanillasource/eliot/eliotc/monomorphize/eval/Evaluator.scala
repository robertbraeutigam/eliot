package com.vanillasource.eliot.eliotc.monomorphize.eval

import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.monomorphize.domain.*
import com.vanillasource.eliot.eliotc.monomorphize.domain.SemValue.*
import com.vanillasource.eliot.eliotc.monomorphize.fact.GroundValue
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedExpression

/** Pure, synchronous NbE evaluator. Evaluates ORE syntax into the semantic domain (SemValue).
  *
  * The evaluator always produces VLam for FunctionLiteral — the Checker is the only place that produces VPi.
  *
  * @param lookupTopDef
  *   Function to look up a top-level definition by ValueFQN, returning its semantic value
  */
class Evaluator(
    lookupTopDef: ValueFQN => Option[SemValue]
) {

  /** Evaluate an ORE expression to a semantic value under the given environment. */
  def eval(env: Env, tm: OperatorResolvedExpression): SemValue = tm match {
    case OperatorResolvedExpression.IntegerLiteral(value) =>
      VConst(GroundValue.Direct(value.value, Evaluator.bigIntGroundType))

    case OperatorResolvedExpression.StringLiteral(value) =>
      VConst(GroundValue.Direct(value.value, Evaluator.stringGroundType))

    case OperatorResolvedExpression.ParameterReference(name) =>
      env
        .lookupByName(name.value)
        .getOrElse(
          VNeutral(NeutralHead.VVar(env.level, name.value), Spine.SNil)
        )

    case OperatorResolvedExpression.ValueReference(vfqn, _) =>
      lookupTopDef(vfqn.value) match {
        case Some(sem) => sem
        case None      => VNeutral(NeutralHead.VVar(env.level, vfqn.value.name.name), Spine.SNil)
      }

    case OperatorResolvedExpression.FunctionApplication(target, arg) =>
      Evaluator.applyValue(eval(env, target.value), eval(env, arg.value))

    case OperatorResolvedExpression.FunctionLiteral(paramName, _, body) =>
      VLam(paramName.value, arg => eval(env.bind(paramName.value, arg), body.value))
  }
}

object Evaluator {

  /** Apply a semantic value to an argument. */
  def applyValue(f: SemValue, x: SemValue): SemValue = f match {
    case VLam(_, closure) => closure(x)

    case VPi(_, codomain) => codomain(x)

    case VNative(_, fire) =>
      val resolved = unfoldTopDef(x)
      resolved match {
        case _: VNeutral => VNeutral(NeutralHead.VVar(0, "native"), Spine.SNil :+ resolved)
        case _           => fire(resolved)
      }

    case VNeutral(head, spine) => VNeutral(head, spine :+ x)

    case VTopDef(fqn, cached, spine) => VTopDef(fqn, cached, spine :+ x)

    case VMeta(id, spine) => VMeta(id, spine :+ x)

    case _ => x // fallback — should not happen in well-typed programs
  }

  /** Unfold a VTopDef by evaluating its cached body and applying the spine. Does not require MetaStore — used at
    * evaluation time before metas exist.
    */
  private def unfoldTopDef(v: SemValue): SemValue = v match {
    case VTopDef(_, Some(cached), spine) =>
      val base   = cached.value
      val result = spine.toList.foldLeft(base)(applyValue)
      unfoldTopDef(result)
    case _                               => v
  }

  /** Force a semantic value by walking solved metas and unfolding VTopDef. */
  def force(v: SemValue, metaStore: MetaStore): SemValue = v match {
    case VMeta(id, spine)                =>
      metaStore.lookup(id) match {
        case Some(solved) =>
          val base = force(solved, metaStore)
          spine.toList.foldLeft(base)(applyValue)
        case None         => v
      }
    case VTopDef(_, Some(cached), spine) =>
      val base   = force(cached.value, metaStore)
      val result = spine.toList.foldLeft(base)(applyValue)
      force(result, metaStore)
    case _                               => v
  }

  import com.vanillasource.eliot.eliotc.module.fact.WellKnownTypes

  /** Convert a SemValue to a GroundValue. Used by native fire functions that need to store type arguments in
    * GroundValue structures.
    */
  def semToGround(v: SemValue): GroundValue = v match {
    case VConst(g)                       => g
    case VType                           => GroundValue.Type
    case VPi(domain, codomain)           =>
      val domGround = semToGround(domain)
      val codGround = semToGround(codomain(VNeutral(NeutralHead.VVar(0, "$quote"), Spine.SNil)))
      GroundValue.Structure(WellKnownTypes.functionDataTypeFQN, Seq(domGround, codGround), GroundValue.Type)
    case VTopDef(fqn, None, spine)       =>
      GroundValue.Structure(fqn, spine.toList.map(semToGround), GroundValue.Type)
    case VTopDef(_, Some(cached), spine) =>
      val base   = cached.value
      val result = spine.toList.foldLeft(base)(applyValue)
      semToGround(result)
    case _                               => GroundValue.Type
  }

  /** Convert a GroundValue to a SemValue. Inverse of [[semToGround]] for [[GroundValue.Structure]]s: the head FQN
    * becomes a [[VTopDef]] and the positional args become a spine in order. `GroundValue.Type` maps to [[VType]]. All
    * other ground values wrap as [[VConst]].
    *
    * Used by ability pattern matching so that concrete query-side ground arguments participate in structural
    * unification against pattern-side [[SemValue]]s produced by evaluating marker-function ORE signatures.
    */
  def groundToSem(g: GroundValue): SemValue = g match {
    case GroundValue.Type                    => VType
    case GroundValue.Structure(fqn, args, _) =>
      args.map(groundToSem).foldLeft(VTopDef(fqn, None, Spine.SNil): SemValue)(applyValue)
    case _                                   => VConst(g)
  }

  /** Ground type for BigInteger values (used by eval for IntegerLiteral). */
  val bigIntGroundType: GroundValue =
    GroundValue.Structure(WellKnownTypes.bigIntFQN, Seq.empty, GroundValue.Type)

  /** Ground type for String values (used by eval for StringLiteral). */
  val stringGroundType: GroundValue =
    GroundValue.Structure(WellKnownTypes.stringFQN, Seq.empty, GroundValue.Type)
}
