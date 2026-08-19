package com.vanillasource.eliot.eliotc.monomorphize.channel

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.monomorphize.fact.{GroundValue, GroundValueRenderer, MonomorphicExpression}
import com.vanillasource.eliot.eliotc.source.content.Sourced

/** The **woven re-check** (effects-as-channel v4, `docs/effects-as-channel-v4.md` §6 "the re-check obligation", §9
  * standing rule 4, §11 P3) — a type check of the body that leaves the codegen seam, run on the woven output itself.
  *
  * '''Why it exists.''' Today elaboration runs *before* the checker precisely so the checker validates the monadic core
  * it emits. v4 moves that lowering *after* monomorphization, which would leave the emitted core machine-generated and
  * unchecked — a silent-miscompile surface the *gaps must be fail-safe* rule does not permit. The mitigation is part of
  * the design, not a follow-up: re-check the woven body. It is ground, so this is definitional equality with **no
  * metavariables, no unification and no meta store** — the cheapest form of checking this compiler has, and a plain
  * structural walk rather than a second checker.
  *
  * '''What it verifies.''' Every ground type annotation the body carries must agree with the shape it annotates:
  *
  *   - a **function literal** is `Function[parameterType, bodyType]`;
  *   - a **function application** applies a function type whose domain is the argument's type and whose codomain is the
  *     application's own type — the rule an inserted `flatMap` chain or `pure` at the wrong instantiation breaks;
  *   - a **parameter reference** has the type its binder declared;
  *   - a bodied value's **signature** is the type of its body.
  *
  * A **value reference**'s agreement with its callee's own woven signature is deliberately *not* checked, and this is
  * the half of P3 that is left for the lowering to bring with it. It is the one rule needing a **fact read**, which
  * would turn the seam into a callee-first traversal — and the `used` fixtures that exist precisely to prove the
  * codegen driver tolerates a *cyclic* injected value show that demand is not safe to add on its own. §6's demand
  * direction already gives the lowering that read for free (`WovenValue(f)` demands `MonomorphicValue` of the machinery
  * it inserts), so the rule lands there rather than being bolted on here. What is checked meanwhile still catches the
  * lowering's own failure mode: a reference inserted at the wrong instantiation disagrees with the application it sits
  * in.
  *
  * '''Landed on today's output first''' (§11 P3), where it must be a no-op: the v3 elaborator writes the same monadic
  * core the v4 lowering will, so a re-check that rejects today's weave is rejecting the elaborator, not the lowering.
  * It is therefore on by default and reports a **hard error** — `assertNoIdResidue` is the precedent for a mandatory
  * tripwire at this seam.
  */
object WovenRecheck {

  /** One disagreement found in a woven body: a message, at the position of the offending node. */
  final case class Problem(at: Sourced[?], message: String)

  /** Re-check a woven value. Returns every disagreement found; empty means the weave type-checks. */
  def check(
      signature: GroundValue,
      runtime: Option[Sourced[MonomorphicExpression.Expression]]
  ): Seq[Problem] =
    runtime.toSeq.flatMap { body =>
      val node = body.map(MonomorphicExpression(signature, _))
      checkNode(node, Map.empty)
    }

  private def checkNode(
      node: Sourced[MonomorphicExpression],
      env: Map[String, GroundValue]
  ): Seq[Problem] = {
    val me = node.value
    me.expression match {
      case MonomorphicExpression.FunctionLiteral(parameterName, parameterType, body) =>
        val inner    = checkNode(body, env.updated(parameterName.value, parameterType))
        val expected = functionType(parameterType, body.value.expressionType)
        inner ++ disagree(node, me.expressionType, expected, "function literal")

      case MonomorphicExpression.FunctionApplication(target, argument) =>
        val inner = checkNode(target, env) ++ checkNode(argument, env)
        target.value.expressionType.asFunctionType match {
          case None                     =>
            inner :+ Problem(
              target,
              s"woven re-check: applied a non-function of type ${GroundValueRenderer.render(target.value.expressionType)}."
            )
          case Some((domain, codomain)) =>
            inner ++
              disagree(argument, argument.value.expressionType, domain, "argument") ++
              disagree(node, me.expressionType, codomain, "application result")
        }

      case MonomorphicExpression.ParameterReference(parameterName) =>
        env.get(parameterName.value) match {
          // A free parameter is not a re-check failure: a woven body is a *fragment* of its value, and the enclosing
          // binder may be one this walk did not enter (a value whose signature is a function but whose body is a
          // reference, for instance). Only a binder this walk *did* see is checked against.
          case None         => Seq.empty
          case Some(bound) => disagree(node, me.expressionType, bound, s"parameter '${parameterName.value}'")
        }

      case _ => Seq.empty
    }
  }

  private def disagree(
      at: Sourced[?],
      actual: GroundValue,
      expected: GroundValue,
      what: String
  ): Seq[Problem] =
    if (actual == expected) Seq.empty
    else
      Seq(
        Problem(
          at,
          s"woven re-check: $what has type ${GroundValueRenderer.render(actual)} but the weave requires " +
            s"${GroundValueRenderer.render(expected)}."
        )
      )

  private def functionType(domain: GroundValue, codomain: GroundValue): GroundValue =
    GroundValue.Structure(
      com.vanillasource.eliot.eliotc.module.fact.WellKnownTypes.functionDataTypeFQN,
      Seq(domain, codomain),
      GroundValue.Type
    )
}
