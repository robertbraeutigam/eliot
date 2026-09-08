package com.vanillasource.eliot.eliotc.monomorphize.unify

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.effect.EffectRowRendering
import com.vanillasource.eliot.eliotc.module.fact.{Qualifier, ValueFQN, WellKnownTypes}
import com.vanillasource.eliot.eliotc.monomorphize.domain.*
import com.vanillasource.eliot.eliotc.monomorphize.domain.SemValue.*
import com.vanillasource.eliot.eliotc.monomorphize.eval.Evaluator
import com.vanillasource.eliot.eliotc.monomorphize.fact.{GroundValue, GroundValueRenderer}

/** Pretty-prints [[SemValue]]s for user-facing error messages. Forces through the given [[MetaStore]] so that solved
  * metas display their solution.
  *
  * Function types (both [[VPi]] at the semantic level and [[GroundValue.Structure]] encoding `Function[A, B]`) are
  * rendered with arrow notation `A -> B`. A constructor application is bracketed by its head's qualifier — `[]` for a
  * type constructor, `()` for a value constructor — so a type reads `Box[String]` and a value reads `Box("a")`,
  * agreeing with [[GroundValueRenderer]]. Unsolved metas appear as `?N`.
  *
  * '''Effect carriers and `Id` are never printed''' (docs/effects-as-channel.md §9): a carrier stack is inverted back
  * to the **pinned effect row** that spells it (`{Abort | IO} String`, not `AbortCarrier(IO, String)`) and the identity
  * carrier's payload wrapper `Id[X]` prints as `X`. Without this, the `Expected:`/`Actual:` lines of an ordinary
  * mismatch on a carrier-headed judgment hand the user machinery names that no surface syntax even spells — e.g. the
  * documented "discharge the expression, not a `val`-bound binder" limitation reports its expectation as a carrier.
  *
  * Ground values delegate to [[GroundValueRenderer]] (one inverter, one set of `Id` rules). At the *semantic* level
  * there is no `valueType` to say whether a carrier application is applied to its payload, so a layer with two or more
  * arguments is rendered as payload-applied and a one-argument layer as payload-less. That is exact for the type
  * positions this printer serves (an `Expected:`/`Actual:` line is a type, hence fully applied) and, being confined to
  * a message, cosmetic if a future shape falls outside it — never a typing decision (finding 14).
  */
object SemValuePrinter {

  /** Render a semantic value as a readable string, forcing through [[metaStore]]. */
  def show(v: SemValue, metaStore: MetaStore): String = go(v, metaStore, 0, topLevel = true)

  private def go(v: SemValue, metaStore: MetaStore, depth: Int, topLevel: Boolean): String = {
    val forced = Evaluator.force(v, metaStore)
    forced match {
      case VType =>
        "Type"

      case VConst(g) =>
        if (topLevel) GroundValueRenderer.render(g) else GroundValueRenderer.renderOperand(g)

      case VPi(domain, codomain) =>
        val placeholder = VNeutral(NeutralHead.Param(depth, s"$$p$depth"), Spine.SNil)
        val body        = codomain(placeholder)
        val domStr      = go(domain, metaStore, depth, topLevel = false)
        val codStr      = go(body, metaStore, depth + 1, topLevel = true)
        parenIf(!topLevel, s"$domStr -> $codStr")

      case VLam(name, closure) =>
        val placeholder = VNeutral(NeutralHead.Param(depth, name), Spine.SNil)
        val body        = closure(placeholder)
        parenIf(!topLevel, s"$name => ${go(body, metaStore, depth + 1, topLevel = true)}")

      case VMeta(id, spine) =>
        val args = spine.toList.map(go(_, metaStore, depth, topLevel = false))
        if (args.isEmpty) s"?${id.value}"
        else s"?${id.value}(${args.mkString(", ")})"

      case VNeutral(head, spine) =>
        applied(head.name, spine, metaStore, depth)

      case VTopDef(fqn, _, spine, _) =>
        showHeaded(fqn, spine, metaStore, depth)

      case VStuckNative(fqn, spine) =>
        showHeaded(fqn, spine, metaStore, depth)

      case VNative(_, _) =>
        "<native>"
    }
  }

  /** An FQN-headed application: the identity carrier's payload wrapper prints as its payload, a canonical carrier stack
    * as its pinned row, anything else as `name[args]` for a **type constructor** ([[Qualifier.Type]]) and `name(args)`
    * for a **value constructor**, matching how the user writes them (`Box[String]` the type, `Box("a")` the value) and
    * agreeing with [[com.vanillasource.eliot.eliotc.monomorphize.fact.GroundValueRenderer]] on the ground side.
    */
  private def showHeaded(fqn: ValueFQN, spine: Spine, metaStore: MetaStore, depth: Int): String = {
    val args = spine.toList
    args match {
      case payload +: _ if fqn === WellKnownTypes.idFQN => go(payload, metaStore, depth, topLevel = true)
      case _                                            =>
        layerOf(fqn, args)
          .map(EffectRowRendering.row(_, peel(metaStore, depth), render(metaStore, depth)))
          .getOrElse(appliedHeaded(fqn, spine, metaStore, depth))
    }
  }

  private def layerOf(fqn: ValueFQN, args: Seq[SemValue]): Option[EffectRowRendering.Layer[SemValue]] =
    EffectRowRendering.layerOf(fqn, args, appliedToPayload = args.sizeIs >= 2)

  private def peel(metaStore: MetaStore, depth: Int)(value: SemValue): Option[EffectRowRendering.Layer[SemValue]] =
    Evaluator.force(value, metaStore) match {
      case VTopDef(fqn, _, spine, _) => EffectRowRendering.layerOf(fqn, spine.toList, appliedToPayload = false)
      case VStuckNative(fqn, spine)  => EffectRowRendering.layerOf(fqn, spine.toList, appliedToPayload = false)
      case _                         => None
    }

  private def render(metaStore: MetaStore, depth: Int)(value: SemValue): String =
    go(value, metaStore, depth, topLevel = true)

  private def applied(name: String, spine: Spine, metaStore: MetaStore, depth: Int): String = {
    val args = spine.toList.map(go(_, metaStore, depth, topLevel = false))
    if (args.isEmpty) name else s"$name(${args.mkString(", ")})"
  }

  /** Like [[applied]], but bracketed by the head's qualifier: `[]` for a type constructor, `()` otherwise. */
  private def appliedHeaded(fqn: ValueFQN, spine: Spine, metaStore: MetaStore, depth: Int): String = {
    val name = fqn.name.name
    val args = spine.toList.map(go(_, metaStore, depth, topLevel = false))
    if (args.isEmpty) name
    else if (fqn.name.qualifier === Qualifier.Type) s"$name[${args.mkString(", ")}]"
    else s"$name(${args.mkString(", ")})"
  }

  private def parenIf(cond: Boolean, s: String): String =
    if (cond) s"($s)" else s
}
