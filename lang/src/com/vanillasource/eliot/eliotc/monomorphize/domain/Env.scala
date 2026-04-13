package com.vanillasource.eliot.eliotc.monomorphize.domain

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

  /** Look up a value by name, returning the most recently bound value with that name. Used by the evaluator to resolve
    * parameters bound by FunctionLiteral closures.
    */
  def lookupByName(name: String): Option[SemValue] = {
    val idx = names.lastIndexOf(name)
    if (idx >= 0) Some(bindings(idx)) else None
  }
}

object Env {
  val empty: Env = Env(Vector.empty, Vector.empty)
}
