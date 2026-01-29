package com.vanillasource.eliot.eliotc.eval.fact

/** The evaluator creates these values to represent every "runtime" value in a program.
  */
trait Value

object Value {
  case class LiteralString(value: String)          extends Value
  case class LiteralInteger(value: BigInt)         extends Value
  case class Structure(values: Map[String, Value]) extends Value
}
