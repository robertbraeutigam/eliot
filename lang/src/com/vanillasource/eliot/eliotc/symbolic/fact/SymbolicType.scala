package com.vanillasource.eliot.eliotc.symbolic.fact

import cats.Show
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.eval.fact.Types
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.pos.PositionRange
import com.vanillasource.eliot.eliotc.source.content.Sourced

import java.net.URI
import scala.annotation.tailrec

sealed trait SymbolicType

object SymbolicType {

  private val syntheticUri = URI.create("")

  def unsourced[T](value: T): Sourced[T] = Sourced(syntheticUri, PositionRange.zero, value)

  /** A reference to a named type (data type or Type itself). */
  case class TypeReference(vfqn: ValueFQN) extends SymbolicType

  /** A type variable (either a unification variable or a universal variable). */
  case class TypeVariable(name: String) extends SymbolicType

  /** A type-level lambda (introduces a type parameter). */
  case class TypeLambda(parameterName: String, body: Sourced[SymbolicType]) extends SymbolicType {
    override def equals(that: Any): Boolean = that match {
      case TypeLambda(n, b) => parameterName == n && body.value == b.value
      case _                => false
    }
    override def hashCode(): Int = (parameterName, body.value).hashCode()
  }

  /** A type-level application (applying a type constructor to an argument). */
  case class TypeApplication(target: Sourced[SymbolicType], argument: Sourced[SymbolicType]) extends SymbolicType {
    override def equals(that: Any): Boolean = that match {
      case TypeApplication(t, a) => target.value == t.value && argument.value == a.value
      case _                     => false
    }
    override def hashCode(): Int = (target.value, argument.value).hashCode()
  }

  /** A literal type (e.g. a specific integer or string value used at the type level). */
  case class LiteralType(value: Any, typeFQN: ValueFQN) extends SymbolicType

  // --- Function type helpers ---

  def functionType(paramType: SymbolicType, returnType: SymbolicType): SymbolicType =
    TypeApplication(
      unsourced(TypeApplication(unsourced(TypeReference(Types.functionDataTypeFQN)), unsourced(paramType))),
      unsourced(returnType)
    )

  object FunctionType {
    def unapply(st: SymbolicType): Option[(SymbolicType, SymbolicType)] =
      st match {
        case TypeApplication(target, returnType) if isFunctionApplicationOfRef(target.value) =>
          target.value match {
            case TypeApplication(_, paramType) => Some((paramType.value, returnType.value))
            case _                             => None
          }
        case _                                                                               => None
      }

    private def isFunctionApplicationOfRef(st: SymbolicType): Boolean =
      st match {
        case TypeApplication(target, _) =>
          target.value match {
            case TypeReference(vfqn) => vfqn === Types.functionDataTypeFQN
            case _                   => false
          }
        case _                          => false
      }
  }

  // --- Structural operations ---

  def containsVar(st: SymbolicType, varName: String): Boolean =
    st match {
      case TypeVariable(name)            => name == varName
      case TypeApplication(target, arg)  => containsVar(target.value, varName) || containsVar(arg.value, varName)
      case TypeLambda(_, body)           => containsVar(body.value, varName)
      case _                             => false
    }

  def transform(st: SymbolicType, f: SymbolicType => SymbolicType): SymbolicType =
    f(st match {
      case TypeApplication(target, arg)     =>
        TypeApplication(target.map(transform(_, f)), arg.map(transform(_, f)))
      case TypeLambda(name, body)           =>
        TypeLambda(name, body.map(transform(_, f)))
      case leaf                             => leaf
    })

  def substitute(body: SymbolicType, paramName: String, argValue: SymbolicType): SymbolicType =
    body match {
      case TypeVariable(name) if name == paramName            => argValue
      case TypeVariable(_)                                    => body
      case TypeApplication(target, arg)                       =>
        TypeApplication(target.map(substitute(_, paramName, argValue)), arg.map(substitute(_, paramName, argValue)))
      case TypeLambda(name, inner) if name != paramName       =>
        TypeLambda(name, inner.map(substitute(_, paramName, argValue)))
      case _                                                  => body
    }

  def betaReduce(st: SymbolicType): SymbolicType =
    st match {
      case TypeApplication(target, arg)     =>
        val reducedTarget = betaReduce(target.value)
        val reducedArg    = betaReduce(arg.value)
        reducedTarget match {
          case TypeLambda(paramName, body) =>
            betaReduce(substitute(body.value, paramName, reducedArg))
          case _                           =>
            TypeApplication(target.as(reducedTarget), arg.as(reducedArg))
        }
      case TypeLambda(name, body)           =>
        TypeLambda(name, body.as(betaReduce(body.value)))
      case other                            => other
    }

  @tailrec
  def stripLeadingApplications(st: SymbolicType): SymbolicType =
    st match {
      case TypeApplication(target, _) => stripLeadingApplications(target.value)
      case other                      => other
    }

  @tailrec
  def stripUniversalTypeIntros(st: SymbolicType): SymbolicType =
    st match {
      case TypeLambda(_, body) => stripUniversalTypeIntros(body.value)
      case other               => other
    }

  def extractLeadingLambdaParams(st: SymbolicType): Seq[String] =
    st match {
      case TypeLambda(name, body) => name +: extractLeadingLambdaParams(body.value)
      case _                      => Seq.empty
    }

  def matchTypes(
      pattern: SymbolicType,
      concrete: SymbolicType,
      isTypeVar: String => Boolean = _ => true
  ): Map[String, SymbolicType] =
    (pattern, concrete) match {
      case (TypeVariable(name), _) if isTypeVar(name)                                 =>
        Map(name -> concrete)
      case (FunctionType(p1, r1), FunctionType(p2, r2))                               =>
        matchTypes(p1, p2, isTypeVar) ++ matchTypes(r1, r2, isTypeVar)
      case (TypeApplication(t1, a1), TypeApplication(t2, a2))                         =>
        matchTypes(t1.value, t2.value, isTypeVar) ++ matchTypes(a1.value, a2.value, isTypeVar)
      case (TypeLambda(_, patBody), TypeLambda(_, tgtBody))                           =>
        matchTypes(patBody.value, tgtBody.value, isTypeVar)
      case _                                                                           => Map.empty
    }

  // --- Show instances ---

  given Show[SymbolicType] with {
    def show(st: SymbolicType): String = st match {
      case FunctionType(paramType, returnType) => s"Function(${paramType.show}, ${returnType.show})"
      case TypeReference(vfqn)                 => vfqn.show
      case TypeVariable(name)                  => name
      case TypeLambda(name, body)              => s"(($name) -> ${body.value.show})"
      case TypeApplication(target, arg)        => s"${target.value.show}(${arg.value.show})"
      case LiteralType(value, _)               => value.toString
    }
  }

  val symbolicTypeUserDisplay: Show[SymbolicType] = {
    case FunctionType(paramType, returnType) =>
      s"${symbolicTypeUserDisplay.show(paramType)} -> ${symbolicTypeUserDisplay.show(returnType)}"
    case TypeReference(vfqn)                 => vfqn.name.name
    case TypeVariable(name)                  => name
    case TypeLambda(name, body)              =>
      s"($name) -> ${symbolicTypeUserDisplay.show(body.value)}"
    case TypeApplication(target, arg)        =>
      s"${symbolicTypeUserDisplay.show(target.value)}(${symbolicTypeUserDisplay.show(arg.value)})"
    case LiteralType(value, _)               => value.toString
  }
}
