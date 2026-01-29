package com.vanillasource.eliot.eliotc.eval.fact

/** The evaluator creates these values to represent every "runtime" value in a program.
  */
trait Value {
  val valueType: Value
}

object Value {
  case class Direct(value: Any, valueType: Value)                    extends Value
  case class Structure(fields: Map[String, Value], valueType: Value) extends Value
}
