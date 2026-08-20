package com.vanillasource.eliot.eliotc.resolve.fact

import cats.Applicative
import cats.syntax.all.*

/** One `~` ability constraint on a generic binder, with its ability name resolved — `F ~ Console[F]` as every phase
  * from resolution onwards carries it.
  *
  * It is parametric in the phase's expression type exactly as [[com.vanillasource.eliot.eliotc.ast.fact.EffectRow]] is
  * parametric in the constraint type, so `E` is this package's `Expression` at resolve and block desugaring, then
  * `MatchDesugaredExpression`, then `OperatorResolvedExpression` — and each fact-chain hop is a [[map]] or a
  * [[traverse]] rather than a per-phase copy of the same two fields.
  *
  * The pre-resolution spelling is [[com.vanillasource.eliot.eliotc.ast.fact.UnresolvedAbilityConstraint]], which
  * carries the combinator (`&`) this one deliberately does not.
  */
case class AbilityConstraint[E](abilityFQN: AbilityFQN, typeArgs: Seq[E]) {

  /** Convert every type argument with a pure function — the pure fact-chain hops (block→matchdesugar). */
  def map[F](f: E => F): AbilityConstraint[F] = AbilityConstraint(abilityFQN, typeArgs.map(f))

  /** Convert every type argument with an effectful function — the resolving hops (matchdesugar→operator). */
  def traverse[G[_]: Applicative, F](f: E => G[F]): G[AbilityConstraint[F]] =
    typeArgs.traverse(f).map(AbilityConstraint(abilityFQN, _))
}
