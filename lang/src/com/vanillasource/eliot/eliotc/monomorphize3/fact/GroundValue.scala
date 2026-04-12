package com.vanillasource.eliot.eliotc.monomorphize3.fact

import com.vanillasource.eliot.eliotc.module.fact.ValueFQN

/** Ground values represent fully evaluated, concrete values with no free variables or unsolved metas. These are the
  * output of quoting NbE semantic values back to a first-order representation.
  *
  * Examples:
  *   - `Direct(42, bigIntType)` — the integer 42 with type BigInteger
  *   - `Structure(Map("A" -> intType, "B" -> stringType, "$typeName" -> ...), Type)` — Function[Int, String]
  *   - `Type` — the type of all types
  */
sealed trait GroundValue {
  def valueType: GroundValue
}

object GroundValue {
  case class Direct(value: Any, override val valueType: GroundValue)                    extends GroundValue
  case class Structure(fields: Map[String, GroundValue], override val valueType: GroundValue) extends GroundValue

  /** The type of all types and Type itself. This is the root of the type hierarchy where the infinite recursion stops.
    */
  object Type extends GroundValue {
    override def valueType: GroundValue = this
  }
}
