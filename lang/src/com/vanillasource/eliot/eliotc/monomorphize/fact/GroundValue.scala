package com.vanillasource.eliot.eliotc.monomorphize.fact

import cats.{Eq, Show}
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.module.fact.{ValueFQN, WellKnownTypes}

/** Ground values represent fully evaluated, concrete values with no free variables or unsolved metas. These are the
  * output of quoting NbE semantic values back to a first-order representation.
  *
  * Examples:
  *   - `Direct(42, bigIntType)` — the integer 42 with type BigInteger
  *   - `Structure(functionFQN, Seq(intType, stringType), Type)` — Function[Int, String]
  *   - `Type` — the type of all types
  */
sealed trait GroundValue {
  def valueType: GroundValue
}

object GroundValue {
  case class Direct(value: Any, override val valueType: GroundValue) extends GroundValue

  /** A type-constructor or data-constructor application identified by [[typeName]] applied to positional [[args]]. */
  case class Structure(typeName: ValueFQN, args: Seq[GroundValue], override val valueType: GroundValue)
      extends GroundValue

  /** The type of all types and Type itself. This is the root of the type hierarchy where the infinite recursion stops.
    */
  object Type extends GroundValue {
    override def valueType: GroundValue = this
  }

  extension (gv: GroundValue) {

    def typeFQN: Option[ValueFQN] =
      gv match {
        case Structure(name, _, _) => Some(name)
        case Type                  => Some(WellKnownTypes.typeFQN)
        case _                     => None
      }

    def asFunctionType: Option[(GroundValue, GroundValue)] =
      gv match {
        case Structure(name, Seq(a, b), Type) if name === WellKnownTypes.functionDataTypeFQN => Some((a, b))
        case _                                                                               => None
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
    case Type                                     => "Type"
    case Direct(value, _)                         => value.toString
    case Structure(name, _, valueType) if valueType === Type => name.name.name
    case Structure(_, _, _)                       => "Structure(...)"
  }
}
