package com.vanillasource.eliot.eliotc.monomorphize.fact

import cats.{Eq, Show}
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.eval.fact.{Types, Value}
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

  /** Convert a GroundValue to the eval package's Value type. Used for looking up AbilityImplementation facts which are
    * keyed by eval.fact.Value.
    */
  def toEvalValue(gv: GroundValue): Value = gv match {
    case Direct(value: ValueFQN, _)        => Value.Direct(value, Types.fullyQualifiedNameType)
    case Direct(value, valueType)          => Value.Direct(value, toEvalValue(valueType))
    case Structure(fields, valueType)      => Value.Structure(fields.map((k, v) => (k, toEvalValue(v))), toEvalValue(valueType))
    case Type                              => Value.Type
  }

  extension (gv: GroundValue) {

    def typeFQN: Option[ValueFQN] =
      gv match {
        case Structure(fields, _) =>
          fields.get("$typeName").collect { case Direct(vfqn: ValueFQN, _) => vfqn }
        case Type                 => Some(Types.typeFQN)
        case _                    => None
      }

    def asFunctionType: Option[(GroundValue, GroundValue)] =
      gv match {
        case Structure(fields, Type) =>
          fields.get("$typeName").collect {
            case Direct(vfqn: ValueFQN, _) if vfqn === Types.functionDataTypeFQN =>
              (fields("A"), fields("B"))
          }
        case _                       => None
      }

    @scala.annotation.tailrec
    def deepReturnType: GroundValue =
      gv.asFunctionType match {
        case Some((_, returnType)) => returnType.deepReturnType
        case None                  => gv
      }

    def functionArity: Int =
      gv.asFunctionType match {
        case Some((_, returnType)) => 1 + returnType.functionArity
        case None                  => 0
      }

    def extractParamAndReturnTypes: (Seq[GroundValue], GroundValue) =
      gv.asFunctionType match {
        case Some((paramType, returnType)) =>
          val (restParams, finalReturn) = returnType.extractParamAndReturnTypes
          (paramType +: restParams, finalReturn)
        case None                          =>
          (Seq.empty, gv)
      }
  }

  given Eq[GroundValue] = Eq.fromUniversalEquals

  given Show[GroundValue] = {
    case Type                                               => "Type"
    case Direct(value, _)                                   => value.toString
    case Structure(fields, valueType) if valueType === Type =>
      fields
        .get("$typeName")
        .map(_.asInstanceOf[Direct].value.asInstanceOf[ValueFQN].name.name)
        .getOrElse("<unknown type>")
    case Structure(_, _)                                    => "Structure(...)"
  }
}
