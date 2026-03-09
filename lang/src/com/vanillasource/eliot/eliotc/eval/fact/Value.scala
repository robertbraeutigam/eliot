package com.vanillasource.eliot.eliotc.eval.fact

import cats.{Eq, Show}
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.eval.fact.Types.{fullyQualifiedNameType, typeFQN}
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, ValueFQN}

/** The evaluator creates these values to represent every "runtime" value in a program.
  */
sealed trait Value {
  def valueType: Value
}

object Value {
  case class Direct(value: Any, override val valueType: Value)                    extends Value
  case class Structure(fields: Map[String, Value], override val valueType: Value) extends Value

  /** The type of all types and Type itself. This is the root of the type hierarchy where the infinite recursion stops.
    * Both its valueType and all field types reference itself.
    */
  object Type extends Value {
    override def valueType: Value = this
  }

  extension (value: Value) {

    /** If this value represents a type, then return that type's ValueFQN.
      * @return
      */
    def typeFQN: Option[ValueFQN] =
      value match {
        case Value.Structure(fields, _) =>
          fields.get("$typeName").collect { case Value.Direct(vfqn: ValueFQN, _) => vfqn }
        case Value.Type                 => Some(Types.typeFQN)
        case _                          => None
      }
  }

  def sameType(left: Value, right: Value): Boolean =
    (left, right) match {
      case (Type, Type) => true
      case (Structure(leftFields, _), Structure(rightFields, _))
          if leftFields.contains("$typeName") && rightFields.contains("$typeName") =>
        leftFields.get("$typeName") === rightFields.get("$typeName")
      case _            => false
    }

  given Eq[Value] = Eq.fromUniversalEquals

  given valueUserDisplay: Show[Value] = {
    case Type                                               => "Type"
    case Direct(value, valueType)                           => value.toString // TODO: this should be explicit
    case Structure(fields, valueType) if valueType === Type =>
      fields
        .get("$typeName")
        .map(_.asInstanceOf[Direct].value.asInstanceOf[ValueFQN].name.name)
        .getOrElse("<unknown type>")
    case Structure(fields, valueType)                       => "Structure(...)"
  }
}
