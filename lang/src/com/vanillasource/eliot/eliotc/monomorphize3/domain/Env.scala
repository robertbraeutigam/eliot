package com.vanillasource.eliot.eliotc.monomorphize3.domain

/** De Bruijn level-indexed environment for NbE evaluation. Variable lookup is by level (index into `bindings`).
  * Name-to-level resolution happens at Checker time, not inside the Evaluator.
  *
  * @param bindings
  *   Values indexed by de Bruijn level
  * @param names
  *   Names for each level (parallel to bindings, used for debugging/quoting)
  */
case class Env(bindings: Vector[SemValue], names: Vector[String]) {

  /** Current de Bruijn level (= number of bindings). */
  def level: Int = bindings.length

  /** Bind a new variable at the next level. */
  def bind(name: String, value: SemValue): Env =
    Env(bindings :+ value, names :+ name)

  /** Look up a value by its de Bruijn level. */
  def lookupByLevel(lvl: Int): SemValue = bindings(lvl)
}

object Env {
  val empty: Env = Env(Vector.empty, Vector.empty)
}
