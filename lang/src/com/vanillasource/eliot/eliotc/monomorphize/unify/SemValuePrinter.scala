package com.vanillasource.eliot.eliotc.monomorphize.unify

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.monomorphize.domain.*
import com.vanillasource.eliot.eliotc.monomorphize.domain.SemValue.*
import com.vanillasource.eliot.eliotc.monomorphize.eval.Evaluator
import com.vanillasource.eliot.eliotc.monomorphize.fact.GroundValue

/** Pretty-prints [[SemValue]]s for user-facing error messages. Forces through the given [[MetaStore]] so that solved
  * metas display their solution.
  *
  * Function types (both [[VPi]] at the semantic level and [[GroundValue.Structure]] encoding `Function[A, B]`) are
  * rendered with arrow notation `A -> B`. Unsolved metas appear as `?N`.
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
        showGround(g, topLevel)

      case VPi(domain, codomain) =>
        val placeholder = VNeutral(NeutralHead.VVar(depth, s"$$p$depth"), Spine.SNil)
        val body        = codomain(placeholder)
        val domStr      = go(domain, metaStore, depth, topLevel = false)
        val codStr      = go(body, metaStore, depth + 1, topLevel = true)
        parenIf(!topLevel, s"$domStr -> $codStr")

      case VLam(name, closure) =>
        val placeholder = VNeutral(NeutralHead.VVar(depth, name), Spine.SNil)
        val body        = closure(placeholder)
        parenIf(!topLevel, s"$name => ${go(body, metaStore, depth + 1, topLevel = true)}")

      case VMeta(id, spine) =>
        val args = spine.toList.map(go(_, metaStore, depth, topLevel = false))
        if (args.isEmpty) s"?${id.value}"
        else s"?${id.value}(${args.mkString(", ")})"

      case VNeutral(NeutralHead.VVar(_, name), spine) =>
        val args = spine.toList.map(go(_, metaStore, depth, topLevel = false))
        if (args.isEmpty) name
        else s"$name(${args.mkString(", ")})"

      case VTopDef(fqn, _, spine) =>
        val args = spine.toList.map(go(_, metaStore, depth, topLevel = false))
        if (args.isEmpty) fqn.name.name
        else s"${fqn.name.name}(${args.mkString(", ")})"

      case VNative(_, _) =>
        "<native>"
    }
  }

  private def showGround(g: GroundValue, topLevel: Boolean): String =
    g match {
      case GroundValue.Type                         => "Type"
      case GroundValue.Direct(value, _)             => value.toString
      case GroundValue.Structure(typeName, args, _) =>
        g.asFunctionType match {
          case Some((a, b)) =>
            parenIf(!topLevel, s"${showGround(a, topLevel = false)} -> ${showGround(b, topLevel = true)}")
          case None         =>
            if (args.isEmpty) typeName.name.name
            else s"${typeName.name.name}[${args.map(showGround(_, topLevel = true)).mkString(", ")}]"
        }
    }

  private def parenIf(cond: Boolean, s: String): String =
    if (cond) s"($s)" else s
}
