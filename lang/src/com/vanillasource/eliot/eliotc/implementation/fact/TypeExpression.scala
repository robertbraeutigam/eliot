package com.vanillasource.eliot.eliotc.implementation.fact

import cats.Show
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.module.fact.{ValueFQN, WellKnownTypes}
import com.vanillasource.eliot.eliotc.monomorphize.fact.GroundValue
import com.vanillasource.eliot.eliotc.source.content.Sourced

import scala.annotation.tailrec

/** Type-level expression used for structural matching of ability implementations. Supports free variables
  * (ParameterReference) for pattern matching with variable capture.
  */
sealed trait TypeExpression

object TypeExpression {

  case class ConcreteValue(value: GroundValue) extends TypeExpression

  case class FunctionLiteral(
      parameterName: String,
      parameterType: GroundValue,
      body: Sourced[TypeExpression]
  ) extends TypeExpression {
    override def equals(that: Any): Boolean = that match {
      case FunctionLiteral(n, t, b) => parameterName == n && parameterType == t && body.value == b.value
      case _                        => false
    }
    override def hashCode(): Int            = (parameterName, parameterType, body.value).hashCode()
  }

  case class ParameterReference(parameterName: String) extends TypeExpression

  case class FunctionApplication(
      target: Sourced[TypeExpression],
      argument: Sourced[TypeExpression]
  ) extends TypeExpression {
    override def equals(that: Any): Boolean = that match {
      case FunctionApplication(t, a) => target.value == t.value && argument.value == a.value
      case _                         => false
    }
    override def hashCode(): Int            = (target.value, argument.value).hashCode()
  }

  @tailrec
  def stripLeadingLambdas(expr: TypeExpression): TypeExpression =
    expr match {
      case FunctionLiteral(_, _, body) => stripLeadingLambdas(body.value)
      case other                       => other
    }

  def extractLeadingLambdaParams(expr: TypeExpression): Seq[(String, GroundValue)] =
    expr match {
      case FunctionLiteral(name, paramType, body) => (name, paramType) +: extractLeadingLambdaParams(body.value)
      case _                                      => Seq.empty
    }

  /** Walk a curried function type (`A -> B -> ... -> R`) and collect the argument types (`A`, `B`, ...),
    * dropping the final return type. Used to extract pattern argument types from an ability implementation's
    * marker function signature.
    */
  def extractFunctionArgTypes(expr: TypeExpression): Seq[TypeExpression] =
    expr match {
      case FunctionType(paramType, returnType) => paramType +: extractFunctionArgTypes(returnType)
      case _                                   => Seq.empty
    }

  def concreteValueOf(expr: TypeExpression): Option[GroundValue] =
    expr match {
      case ConcreteValue(value) => Some(value)
      case _                    => None
    }

  /** Convert a concrete GroundValue into application-chain form. Type structures with type arguments are decomposed
    * into FunctionApplication chains for structural matching.
    */
  def fromGroundValue(gv: GroundValue, source: Sourced[?]): TypeExpression =
    gv match {
      case GroundValue.Structure(fields, GroundValue.Type) =>
        fields.get("$typeName") match {
          case Some(typeName @ GroundValue.Direct(_: ValueFQN, _)) =>
            val typeArgFields          = fields.removed("$typeName")
            val base: TypeExpression = ConcreteValue(GroundValue.Structure(Map("$typeName" -> typeName), GroundValue.Type))
            if (typeArgFields.isEmpty) base
            else
              typeArgFields.toSeq.sortBy(_._1).foldLeft(base) { case (acc, (_, argValue)) =>
                FunctionApplication(source.as(acc), source.as(fromGroundValue(argValue, source)))
              }
          case _                                                   => ConcreteValue(gv)
        }
      case _                                               => ConcreteValue(gv)
    }

  /** Capture-avoiding substitution. */
  def substitute(body: TypeExpression, paramName: String, argValue: TypeExpression): TypeExpression =
    body match {
      case ParameterReference(name) if name == paramName                    => argValue
      case ParameterReference(_)                                            => body
      case FunctionApplication(target, arg)                                 =>
        FunctionApplication(target.map(substitute(_, paramName, argValue)), arg.map(substitute(_, paramName, argValue)))
      case FunctionLiteral(name, paramType, innerBody) if name != paramName =>
        FunctionLiteral(name, paramType, innerBody.map(substitute(_, paramName, argValue)))
      case _                                                                => body
    }

  /** Beta-reduce all FunctionApplication(FunctionLiteral, arg) redexes. */
  def betaReduce(expr: TypeExpression): TypeExpression =
    expr match {
      case FunctionApplication(target, arg)       =>
        betaReduce(target.value) match {
          case FunctionLiteral(name, _, body) =>
            betaReduce(substitute(body.value, name, betaReduce(arg.value)))
          case reducedTarget                  =>
            FunctionApplication(target.as(reducedTarget), arg.map(betaReduce))
        }
      case FunctionLiteral(name, paramType, body) =>
        FunctionLiteral(name, paramType, body.map(betaReduce))
      case other                                  => other
    }

  /** Match a pattern against a concrete TypeExpression, returning bindings for type variables. */
  def matchTypes(
      pattern: TypeExpression,
      concrete: TypeExpression,
      isTypeVar: String => Boolean = _ => true
  ): Map[String, TypeExpression] =
    (pattern, concrete) match {
      case (ParameterReference(name), _) if isTypeVar(name)                 =>
        Map(name -> concrete)
      case (FunctionType(p1, r1), FunctionType(p2, r2))                     =>
        matchTypes(p1, p2, isTypeVar) ++ matchTypes(r1, r2, isTypeVar)
      case (FunctionApplication(t1, a1), FunctionApplication(t2, a2))       =>
        matchTypes(t1.value, t2.value, isTypeVar) ++ matchTypes(a1.value, a2.value, isTypeVar)
      case (FunctionLiteral(_, _, patBody), FunctionLiteral(_, _, tgtBody)) =>
        matchTypes(patBody.value, tgtBody.value, isTypeVar)
      case (fa: FunctionApplication, ConcreteValue(v @ GroundValue.Structure(fields, GroundValue.Type)))
          if fields.size > 1 && fields.contains("$typeName") =>
        matchTypes(pattern, fromGroundValue(v, fa.target), isTypeVar)
      case (ConcreteValue(v @ GroundValue.Structure(fields, GroundValue.Type)), fa: FunctionApplication)
          if fields.size > 1 && fields.contains("$typeName") =>
        matchTypes(fromGroundValue(v, fa.target), concrete, isTypeVar)
      case _                                                                => Map.empty
    }

  private object FunctionType {
    def unapply(expr: TypeExpression): Option[(TypeExpression, TypeExpression)] =
      expr match {
        case FunctionApplication(target, returnType) if isFunctionApplicationOfDataType(target.value) =>
          target.value match {
            case FunctionApplication(_, paramType) => Some((paramType.value, returnType.value))
            case _                                 => None
          }
        case _                                                                                        => None
      }

    private def isFunctionApplicationOfDataType(expr: TypeExpression): Boolean =
      expr match {
        case FunctionApplication(target, _) =>
          target.value match {
            case ConcreteValue(v) => isFunctionDataType(v)
            case _                => false
          }
        case _                              => false
      }

    private def isFunctionDataType(v: GroundValue): Boolean =
      v match {
        case GroundValue.Structure(fields, _) =>
          fields.get("$typeName") match {
            case Some(GroundValue.Direct(vfqn: ValueFQN, _)) => vfqn === WellKnownTypes.functionDataTypeFQN
            case _                                            => false
          }
        case _                                => false
      }
  }

  given Show[TypeExpression] with {
    def show(expr: TypeExpression): String = expr match {
      case FunctionType(paramType, returnType)    => s"Function(${paramType.show}, ${returnType.show})"
      case ConcreteValue(v)                       => v.show
      case FunctionLiteral(name, paramType, body) => s"(($name: ${paramType.show}) -> ${body.value.show})"
      case ParameterReference(name)               => name
      case FunctionApplication(target, arg)       => s"${target.value.show}(${arg.value.show})"
    }
  }
}
