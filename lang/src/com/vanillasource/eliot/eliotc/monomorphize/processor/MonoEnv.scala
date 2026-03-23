package com.vanillasource.eliot.eliotc.monomorphize.processor

import com.vanillasource.eliot.eliotc.eval.fact.Value

/** Immutable environment threaded through monomorphization. Bundles the type parameter substitution (constant per value
  * being monomorphized) and the runtime parameter types (grows as we enter lambda bodies).
  */
case class MonoEnv(
    typeParamSubst: Map[String, Value],
    runtimeParams: Map[String, Value]
) {
  def withParam(name: String, tpe: Value): MonoEnv =
    copy(runtimeParams = runtimeParams + (name -> tpe))
}
