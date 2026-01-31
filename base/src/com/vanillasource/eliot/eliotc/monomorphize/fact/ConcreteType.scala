package com.vanillasource.eliot.eliotc.monomorphize.fact

import cats.syntax.all.*
import cats.{Eq, Show}
import com.vanillasource.eliot.eliotc.module2.fact.ValueFQN

/** A fully evaluated ground type with no free type variables. This is the output of type evaluation during
  * monomorphization.
  */
sealed trait ConcreteType

object ConcreteType {

  /** A reference to a named type (data types, type aliases).
    */
  case class TypeRef(vfqn: ValueFQN) extends ConcreteType

  /** A function type: paramType -> returnType.
    */
  case class FunctionType(paramType: ConcreteType, returnType: ConcreteType) extends ConcreteType

  /** A type application: target[arg] - for parameterized types.
    */
  case class TypeApplication(target: ConcreteType, argument: ConcreteType) extends ConcreteType

  /** Integer literal type (for dependent typing support).
    */
  case class IntLiteral(value: BigInt) extends ConcreteType

  /** String literal type (for dependent typing support).
    */
  case class StringLiteral(value: String) extends ConcreteType

  given Show[ConcreteType] with {
    def show(t: ConcreteType): String = t match {
      case TypeRef(vfqn)                => vfqn.show
      case FunctionType(param, ret)     => s"(${show(param)}) -> ${show(ret)}"
      case TypeApplication(target, arg) => s"${show(target)}[${show(arg)}]"
      case IntLiteral(n)                => n.toString
      case StringLiteral(s)             => s"\"$s\""
    }
  }

  given Eq[ConcreteType] = Eq.fromUniversalEquals
}
