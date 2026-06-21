package com.vanillasource.eliot.eliotc.effect.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedValue
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.resolve.fact.AbilityFQN
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerError

/** Effect propagation (Decision 6): the user-facing effects a carrier-polymorphic body actually performs (`used`) must
  * be a subset of the effects it declares; an undeclared effect is rejected here with a precise error. Checked only for
  * a value with an abstract effect carrier (a `{...}` set or hand-written `[F[_] ~ ...]`) — a value committing to a
  * concrete carrier (`main : IO[Unit]`) or a pure return has an empty `carrier` and so no declared set to honour (the
  * concrete carrier provides every effect; the pure-return case is the separate fail-safe in
  * [[EffectDesugaringProcessor]]).
  *
  * The check is at *ability* granularity: declaring `{Dep[Database]}` covers any `Dep` use; a finer type-argument
  * mismatch (`Dep[Logger]` where only `Dep[Database]` is declared) is left to monomorphization at the concrete use
  * site, per the use-site-verification cornerstone. This is a structural, definition-local well-formedness check on the
  * effect annotation, not an instantiation-dependent typing obligation.
  */
class DeclaredEffectChecker {

  def verify(
      value: OperatorResolvedValue,
      carrier: Set[String],
      used: Set[AbilityFQN]
  ): CompilerIO[Unit] =
    if (carrier.isEmpty) ().pure[CompilerIO]
    else {
      val declared   = EffectCarriers.declaredEffects(carrier, value.paramConstraints)
      val undeclared = used.diff(declared).toSeq.sortBy(_.abilityName)
      if (undeclared.isEmpty) ().pure[CompilerIO]
      else
        compilerError(
          value.name.as(
            s"This value performs the ${effectWord(undeclared.size)} " +
              undeclared.map(a => s"'${a.abilityName}'").mkString(", ") +
              s" but does not declare ${pronoun(undeclared.size)}; add ${pronoun(undeclared.size)} to its { ... } " +
              "effect set."
          )
        )
    }

  private def effectWord(n: Int): String = if (n == 1) "effect" else "effects"
  private def pronoun(n: Int): String    = if (n == 1) "it" else "them"
}
