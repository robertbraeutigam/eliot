package com.vanillasource.eliot.eliotc.monomorphize3.eval

import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.monomorphize3.domain.*
import com.vanillasource.eliot.eliotc.monomorphize3.domain.SemValue.*
import com.vanillasource.eliot.eliotc.monomorphize3.fact.GroundValue
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedExpression

/** Pure, synchronous NbE evaluator. Evaluates ORE syntax into the semantic domain (SemValue).
  *
  * The evaluator always produces VLam for FunctionLiteral — the Checker is the only place that produces VPi.
  *
  * @param lookupTopDef
  *   Function to look up a top-level definition by ValueFQN, returning its semantic value
  * @param nameLevels
  *   Map from parameter name to de Bruijn level, maintained by the Checker
  */
class Evaluator(
    lookupTopDef: ValueFQN => Option[SemValue],
    nameLevels: Map[String, Int]
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
        .orElse(
          nameLevels.get(name.value).map(env.lookupByLevel)
        )
        .getOrElse(
          VNeutral(NeutralHead.VVar(env.level, name.value), Spine.SNil, VType)
        )

    case OperatorResolvedExpression.ValueReference(vfqn, _) =>
      lookupTopDef(vfqn.value) match {
        case Some(sem) => sem
        case None      => VNeutral(NeutralHead.VVar(env.level, vfqn.value.name.name), Spine.SNil, VType)
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
      x match {
        case _: VNeutral => VNeutral(NeutralHead.VVar(0, "native"), Spine.SNil :+ x, VType)
        case _           => fire(x)
      }

    case VNeutral(head, spine, tpe) => VNeutral(head, spine :+ x, tpe)

    case VTopDef(fqn, cached, spine) => VTopDef(fqn, cached, spine :+ x)

    case VMeta(id, spine, expected) => VMeta(id, spine :+ x, expected)

    case _ => x // fallback — should not happen in well-typed programs
  }

  /** Force a semantic value by walking solved metas and unfolding VTopDef when appropriate. */
  def force(v: SemValue, metaStore: MetaStore): SemValue = v match {
    case VMeta(id, spine, _) =>
      metaStore.lookup(id) match {
        case Some(solved) =>
          val base = force(solved, metaStore)
          spine.toList.foldLeft(base)(applyValue)
        case None         => v
      }
    case _                   => v
  }

  import com.vanillasource.eliot.eliotc.eval.fact.Types

  /** Convert a SemValue to a GroundValue. Used by native fire functions that need to store type arguments in
    * GroundValue structures.
    */
  def semToGround(v: SemValue): GroundValue = v match {
    case VConst(g)             => g
    case VType                 => GroundValue.Type
    case VPi(domain, codomain) =>
      val domGround = semToGround(domain)
      val codGround = semToGround(codomain(VNeutral(NeutralHead.VVar(0, "$quote"), Spine.SNil, VType)))
      GroundValue.Structure(
        Map(
          "$typeName" -> GroundValue.Direct(Types.functionDataTypeFQN, GroundValue.Type),
          "A"         -> domGround,
          "B"         -> codGround
        ),
        GroundValue.Type
      )
    case _                     => GroundValue.Type
  }

  /** Ground type for BigInteger values (used by eval for IntegerLiteral). */
  val bigIntGroundType: GroundValue = GroundValue.Structure(
    Map(
      "$typeName" -> GroundValue.Direct(Types.bigIntFQN, GroundValue.Type)
    ),
    GroundValue.Type
  )

  /** Ground type for String values (used by eval for StringLiteral). */
  val stringGroundType: GroundValue = GroundValue.Structure(
    Map(
      "$typeName" -> GroundValue.Direct(Types.stringFQN, GroundValue.Type)
    ),
    GroundValue.Type
  )
}
