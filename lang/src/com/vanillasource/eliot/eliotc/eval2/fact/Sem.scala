package com.vanillasource.eliot.eliotc.eval2.fact

import com.vanillasource.eliot.eliotc.eval.fact.Value
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedExpression

/** The semantic domain for NbE-based evaluation. Values in weak head normal form.
  *
  * Unlike `Value` from the existing `eval` package, `Sem` carries metavariables as first-class
  * neutral heads and closures for unevaluated lambda bodies.
  */
sealed trait Sem

object Sem {

  /** A literal value: BigInt, String, etc. Wraps the existing eval `Value.Direct`. */
  case class Lit(direct: Value.Direct) extends Sem

  /** The universe of types. */
  case object TypeUniv extends Sem

  /** A fully-evaluated structure (data type instance, function type, etc).
    *
    * @param typeFqn
    *   The fully qualified name of the data type constructor.
    * @param fields
    *   Named fields of the structure, including the `\$typeName` field.
    */
  case class Struct(typeFqn: ValueFQN, fields: Map[String, Sem]) extends Sem

  /** A closure: a lambda value waiting to be applied. The body is ORE — the closure captures the
    * env at construction time and re-evaluates the body when applied.
    */
  case class Lam(paramName: String, dom: Sem, body: Closure) extends Sem

  /** A neutral term: a head (variable, meta, or stuck top-level reference) followed by a spine of
    * arguments. Cannot be reduced further until something fills in the head.
    */
  case class Neut(head: Head, spine: Seq[Sem] = Seq.empty) extends Sem
}

/** The head of a neutral term. */
sealed trait Head

object Head {

  /** A bound parameter that escaped its binder (free in the current scope). */
  case class Param(name: String) extends Head

  /** A unification metavariable. */
  case class Meta(id: MetaId) extends Head

  /** A top-level value reference whose body is currently blocked from reducing (e.g. recursive
    * definition, opaque definition).
    */
  case class Ref(vfqn: ValueFQN) extends Head
}

/** A closure captures an environment and an unevaluated ORE body. When applied, the body is
  * re-evaluated in the closure's environment extended with the argument.
  */
case class Closure(env: Env, body: OperatorResolvedExpression)

/** An environment mapping parameter names to their semantic values. */
case class Env(params: Map[String, Sem] = Map.empty) {
  def extend(name: String, sem: Sem): Env = copy(params = params + (name -> sem))
  def lookup(name: String): Option[Sem]   = params.get(name)
}

object Env {
  def empty: Env = Env()
}

/** Opaque wrapper for metavariable identifiers. */
opaque type MetaId = Int

object MetaId {
  def apply(id: Int): MetaId      = id
  extension (id: MetaId) def raw: Int = id
}
