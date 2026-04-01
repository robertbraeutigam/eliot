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

    /** If this value is a Function[A, B] type, extract (A, B).
      */
    def asFunctionType: Option[(Value, Value)] =
      value match {
        case Value.Structure(fields, Value.Type) =>
          fields.get("$typeName").collect {
            case Value.Direct(vfqn: ValueFQN, _) if vfqn === Types.functionDataTypeFQN =>
              (fields("A"), fields("B"))
          }
        case _                                   => None
      }

    /** Extract the deepest non-function return type by following the B field of nested Function types.
      */
    @scala.annotation.tailrec
    def deepReturnType: Value =
      value.asFunctionType match {
        case Some((_, returnType)) => returnType.deepReturnType
        case None                  => value
      }

    /** Count the function nesting depth (arity).
      */
    def functionArity: Int =
      value.asFunctionType match {
        case Some((_, returnType)) => 1 + returnType.functionArity
        case None                  => 0
      }

    /** Extract all parameter types and the final return type from a curried function type.
      */
    def extractParamAndReturnTypes: (Seq[Value], Value) =
      value.asFunctionType match {
        case Some((paramType, returnType)) =>
          val (restParams, finalReturn) = returnType.extractParamAndReturnTypes
          (paramType +: restParams, finalReturn)
        case None                          =>
          (Seq.empty, value)
      }
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
