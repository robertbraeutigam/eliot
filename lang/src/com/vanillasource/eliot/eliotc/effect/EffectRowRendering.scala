package com.vanillasource.eliot.eliotc.effect

import com.vanillasource.eliot.eliotc.module.fact.ValueFQN

/** Renders a canonical **effect-carrier stack** back as the **pinned effect row** that spells it — the one place that
  * inverts the `{Throw[E], State[S] | Id} A` ⤳ `ThrowCarrier[E, StateCarrier[S, Id], A]` desugar, so no user-facing
  * surface ever prints carrier machinery names.
  *
  * It is generic over the value representation (`A`) because the same inversion is needed at two levels — over ground
  * types (LSP hover, ability-demand diagnostics) and over the checker's semantic values (type-mismatch messages) — and
  * a second copy would be the source that drifts. A caller supplies three things: the head FQN and arguments of a node,
  * a way to `peel` a nested node back into a layer, and a way to `render` a leaf.
  *
  * '''The convention it inverts''' ([[EffectCarrierNaming]]): a carrier application is `<Ability>Carrier[abilityArgs…,
  * base]` as a type *constructor*, plus one trailing payload argument once applied to a result. So the base is always
  * the last argument of the payload-unapplied form, and the ability's own arguments are everything before it — which is
  * why `appliedToPayload` must be supplied by the caller: it is the only thing that decides whether the last argument
  * is the payload or the base.
  *
  * '''Rendering only.''' Layer recognition goes through [[EffectCarrierNaming.abilityNameOfCarrier]], i.e. by *name*.
  * That is sanctioned here and nowhere else: a misrecognition costs a cosmetically wrong type string, whereas the same
  * guess in the checker miscompiles (docs/effects-as-channel.md finding 14).
  */
object EffectRowRendering {

  /** One decomposed carrier layer: the effect it realizes, that effect's own type arguments, the base carrier it sits
    * on, and the payload type — present only when the layer is applied to a result.
    */
  final case class Layer[A](abilityName: String, abilityArgs: Seq[A], base: A, payload: Option[A])

  /** Decompose a node into a carrier layer, or [[None]] if its head is not a canonical carrier or it has too few
    * arguments to carry a base. `appliedToPayload` says whether the last argument is the result type (a full
    * `ThrowCarrier[E, G, A]`) rather than the base (a payload-unapplied `ThrowCarrier[E, G]`, the shape used where a
    * carrier is passed as an `F[_]`).
    */
  def layerOf[A](head: ValueFQN, args: Seq[A], appliedToPayload: Boolean): Option[Layer[A]] = {
    val baseIndex = args.size - (if (appliedToPayload) 2 else 1)
    EffectCarrierNaming
      .abilityNameOfCarrier(head)
      .filter(_ => baseIndex >= 0)
      .map(ability => Layer(ability, args.take(baseIndex), args(baseIndex), Option.when(appliedToPayload)(args.last)))
  }

  /** The carrier a stack ultimately sits on: peel base slot after base slot until the node is no longer a canonical
    * carrier. `IO` for `ThrowCarrier[E, StateCarrier[S, IO]]`, and the node itself for anything that is not a stack.
    *
    * Same *rendering-only* licence as the rest of this object — recognition is by name, so a misrecognition costs a
    * wrong diagnostic, never a wrong compilation.
    */
  @scala.annotation.tailrec
  def baseOf[A](value: A, peel: A => Option[Layer[A]]): A =
    peel(value) match {
      case Some(layer) => baseOf(layer.base, peel)
      case None        => value
    }

  /** Render a layer as its pinned row. Layers nested in the base slot flatten into further entries (leftmost =
    * outermost = discharged first, matching the surface ordering), and the first non-carrier base ends up after the
    * `|`. A payload-bearing layer renders as `{Throw[E] | IO} String`; a payload-less one (a carrier passed as an
    * `F[_]`) as just `{Throw[E] | IO}`.
    *
    * `peel` decomposes a *base-slot* node, so it is asked for the payload-unapplied shape; `render` prints a leaf (an
    * ability argument, the base, the payload).
    */
  def row[A](layer: Layer[A], peel: A => Option[Layer[A]], render: A => String): String = {
    val (entries, base) = flatten(layer, peel, render, Seq.empty)
    val rendered        = s"{${entries.mkString(", ")} | ${render(base)}}"
    layer.payload.fold(rendered)(payload => s"$rendered ${render(payload)}")
  }

  @scala.annotation.tailrec
  private def flatten[A](
      layer: Layer[A],
      peel: A => Option[Layer[A]],
      render: A => String,
      entries: Seq[String]
  ): (Seq[String], A) = {
    val grown = entries :+ entry(layer, render)
    peel(layer.base) match {
      case Some(inner) => flatten(inner, peel, render, grown)
      case None        => (grown, layer.base)
    }
  }

  private def entry[A](layer: Layer[A], render: A => String): String =
    if (layer.abilityArgs.isEmpty) layer.abilityName
    else s"${layer.abilityName}[${layer.abilityArgs.map(render).mkString(", ")}]"
}
