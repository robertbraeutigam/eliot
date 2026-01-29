package com.vanillasource.eliot.eliotc.eval.fact

import com.vanillasource.eliot.eliotc.module2.fact.{ModuleName, ValueFQN}

/** The evaluator creates these values to represent every "runtime" value in a program.
  */
trait Value {
  def valueType: Value
}

object Value {
  case class Direct(value: Any, override val valueType: Value)                    extends Value
  case class Structure(fields: Map[String, Value], override val valueType: Value) extends Value

  /** The type of Type itself. This is the root of the type hierarchy where the infinite recursion stops.
    * Both its valueType and all field types reference itself.
    */
  object TypeType extends Value {
    lazy val fields: Map[String, Value] = Map(
      "$typeName" -> Direct(
        ValueFQN(ModuleName(Seq("eliot", "compile"), "Type"), "Type"),
        TypeType
      )
    )
    override def valueType: Value = this
  }
}
