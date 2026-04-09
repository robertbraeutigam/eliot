package com.vanillasource.eliot.eliotc.eval2.fact

/** An evaluation environment mapping parameter names to their semantic values. */
case class Env(params: Map[String, Sem]) {
  def extend(name: String, sem: Sem): Env = copy(params = params + (name -> sem))
}

object Env {
  def empty: Env = Env(Map.empty)
}
