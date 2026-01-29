package com.vanillasource.eliot.eliotc.eval.fact

/** The evaluator creates these values to represent every "runtime" value in a program.
  */
trait Value {
  def valueType: Value
}

object Value {
  case class Direct(value: Any, override val valueType: Value)                    extends Value
  case class Structure(fields: Map[String, Value], override val valueType: Value) extends Value

  /** A structure with a self-referential type. Used for typeType.
    */
  class SelfTypedStructure(fieldsInit: => Map[String, Value]) extends Value {
    lazy val fields: Map[String, Value]     = fieldsInit
    override def valueType: Value = this
  }
}
