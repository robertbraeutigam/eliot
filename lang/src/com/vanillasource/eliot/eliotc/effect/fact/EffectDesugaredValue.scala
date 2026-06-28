package com.vanillasource.eliot.eliotc.effect.fact

import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedValue
import com.vanillasource.eliot.eliotc.platform.Platform
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}

/** An [[OperatorResolvedValue]] whose *body* has been rewritten from direct style into ordinary monadic form (effects,
  * Part B — the body auto-lift). Where a value declares an effect carrier (a `{E...} A` signature desugars in M1 to a
  * leading inferable higher-kinded carrier `F`, or the return is a concrete carrier like `IO[A]`), every effectful
  * sub-term passed into a pure value-argument position is sequenced with `Monad.flatMap`/`Applicative.map` and a pure
  * body under an effectful return is wrapped with `Monad.pure` — so `println(readLine)` becomes
  * `flatMap(readLine, x -> println(x))`. Code that is already in monadic form (`flatMap(readLine, s -> println(s))`)
  * passes through unchanged (the rewrite is idempotent), and a value with no carrier whose body performs effects is
  * rejected here (it cannot carry them).
  *
  * Only the [[OperatorResolvedValue.runtime]] body changes — the signature (type stack), constraints and every other
  * field are carried through untouched. Like [[com.vanillasource.eliot.eliotc.saturate.fact.SaturatedValue]] this wraps
  * the operator-resolved value rather than rewriting it in place (the `operator` phase produces it upstream, so feeding
  * the rewrite back would be a fact cycle); `SaturatedValueProcessor` consumes this fact in its place.
  *
  * The `flatMap`/`pure`/`map` references are inserted by fully-qualified name (`eliot.lang.Monad`/`eliot.lang.Applicative`),
  * so the user never imports the effect machinery — monomorphization pins the carrier and erases the whole tower.
  *
  * @param value
  *   The operator-resolved value carrying the auto-lifted runtime body.
  */
case class EffectDesugaredValue(value: OperatorResolvedValue) extends CompilerFact {
  override def key(): CompilerFactKey[EffectDesugaredValue] = EffectDesugaredValue.Key(value.vfqn, value.platform)
}

object EffectDesugaredValue {
  case class Key(vfqn: ValueFQN, platform: Platform = Platform.Runtime) extends CompilerFactKey[EffectDesugaredValue]
}
