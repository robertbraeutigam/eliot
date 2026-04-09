package com.vanillasource.eliot.eliotc.eval2.fact

import com.vanillasource.eliot.eliotc.eval.fact.Value
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedExpression
import com.vanillasource.eliot.eliotc.source.content.Sourced

/** The semantic domain for NbE-based evaluation. Values in this domain are in weak head normal form — reduced as far as
  * possible, stopping only at neutral terms (free parameters, stuck top-level references).
  *
  * Compared to the existing `eval` package's `ExpressionValue`, this domain:
  *   - distinguishes neutral terms (stuck computations) as first-class
  *   - uses closures for lambda bodies (capturing the environment)
  *   - is designed to be the canonical normalizer (always reduce, only stop at neutrals)
  */
sealed trait Sem

object Sem {

  /** A literal value: BigInt, String, etc. Wraps the existing eval Value.Direct. */
  case class Lit(direct: Value.Direct) extends Sem

  /** The universe of types. Corresponds to Value.Type. */
  case object TypeUniv extends Sem

  /** A fully-evaluated structure (data type instance, function type, etc). The typeFqn identifies which data type this
    * is, and fields hold the type parameters / fields by name.
    */
  case class Struct(typeFqn: ValueFQN, fields: Map[String, Sem]) extends Sem

  /** A closure: a lambda value waiting to be applied. The body is ORE — the closure captures the env at construction
    * time and re-evaluates the body when applied.
    */
  case class Lam(paramName: String, dom: Sem, body: Closure) extends Sem

  /** A neutral term: a head (variable or stuck top-level reference) followed by a spine of arguments. Cannot be reduced
    * further until something fills in the head.
    */
  case class Neut(head: Head, spine: Seq[Sem]) extends Sem

  /** The head of a neutral term — the part that is blocking reduction. */
  sealed trait Head

  object Head {

    /** A bound parameter that escaped its binder (free in the current scope). */
    case class Param(name: String) extends Head

    /** A top-level value reference whose body is currently blocked from reducing (e.g. recursive definition, opaque
      * definition).
      */
    case class Ref(vfqn: ValueFQN) extends Head
  }

  /** A closure capturing an environment and a body expression. When a Lam is applied to an argument, the body is
    * re-evaluated in the captured environment extended with the argument.
    */
  case class Closure(env: Env, body: Sourced[OperatorResolvedExpression])

  /** An environment mapping parameter names to their semantic values. */
  case class Env(params: Map[String, Sem]) {
    def extend(name: String, sem: Sem): Env = copy(params = params + (name -> sem))
  }

  object Env {
    def empty: Env = Env(Map.empty)
  }
}
