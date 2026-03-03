package com.vanillasource.eliot.eliotc.eval.fact

import cats.Show
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.pos.PositionRange
import com.vanillasource.eliot.eliotc.source.content.Sourced

import java.net.URI
import scala.annotation.tailrec

/** The result of an expression evaluation.
  */
sealed trait ExpressionValue

object ExpressionValue {

  private val syntheticUri = URI.create("")

  def unsourced[T](value: T): Sourced[T] = Sourced(syntheticUri, PositionRange.zero, value)

  /** Generic fold over an ExpressionValue tree. Recurses into children first (bottom-up), then combines with the
    * provided functions.
    */
  def fold[A](
      onConcrete: Value => A,
      onNative: Value => A,
      onParamRef: (String, Value) => A,
      onFunApp: (A, A) => A,
      onFunLit: (String, Value, A) => A
  )(expr: ExpressionValue): A =
    expr match {
      case ConcreteValue(v)                       => onConcrete(v)
      case NativeFunction(paramType, _)           => onNative(paramType)
      case ParameterReference(name, paramType)    => onParamRef(name, paramType)
      case FunctionApplication(target, arg)       =>
        onFunApp(fold(onConcrete, onNative, onParamRef, onFunApp, onFunLit)(target.value),
                 fold(onConcrete, onNative, onParamRef, onFunApp, onFunLit)(arg.value))
      case FunctionLiteral(name, paramType, body) =>
        onFunLit(name, paramType, fold(onConcrete, onNative, onParamRef, onFunApp, onFunLit)(body.value))
    }

  /** Check if an expression contains a variable with the given name. Used for occurs check in unification. */
  def containsVar(expr: ExpressionValue, varName: String): Boolean =
    fold(_ => false, _ => false, (name, _) => name == varName, _ || _, (_, _, inBody) => inBody)(expr)

  /** Strip all leading FunctionLiteral wrappers, returning the innermost body. */
  @tailrec
  def stripLeadingLambdas(expr: ExpressionValue): ExpressionValue =
    expr match {
      case FunctionLiteral(_, _, body) => stripLeadingLambdas(body.value)
      case other                       => other
    }

  @tailrec
  def stripLeadingFunctionApplications(expr: ExpressionValue): ExpressionValue =
    expr match {
      case FunctionApplication(target, _) => stripLeadingFunctionApplications(target.value)
      case other                          => other
    }

  /** Extract parameter names and types from leading FunctionLiteral wrappers. */
  def extractLeadingLambdaParams(expr: ExpressionValue): Seq[(String, Value)] =
    expr match {
      case FunctionLiteral(name, paramType, body) => (name, paramType) +: extractLeadingLambdaParams(body.value)
      case _                                      => Seq.empty
    }

  /** Extract parameter types from a fully-evaluated function type structure. After evaluation, Function[A, B] becomes
    * ConcreteValue(Structure(Map("$typeName"->Function, "A"->paramType, "B"->returnType), Type)). This method extracts
    * parameter types from such structures.
    */
  def extractFunctionTypeParams(expr: ExpressionValue): Seq[Value] =
    expr match {
      case ConcreteValue(Value.Structure(fields, Value.Type))
          if fields.get("$typeName").exists(isFunctionTypeName) =>
        val paramType  = fields("A")
        val returnType = fields("B")
        paramType +: extractFunctionTypeParams(ConcreteValue(returnType))
      case _ => Seq.empty
    }

  private def isFunctionTypeName(value: Value): Boolean =
    value match {
      case Value.Direct(vfqn: ValueFQN, _) => vfqn === Types.functionDataTypeFQN
      case _                               => false
    }

  /** Capture-avoiding substitution: replace all free occurrences of paramName with argValue. */
  def substitute(body: ExpressionValue, paramName: String, argValue: ExpressionValue): ExpressionValue =
    body match {
      case ParameterReference(name, _) if name == paramName                 => argValue
      case ParameterReference(_, _)                                         => body
      case FunctionApplication(target, arg)                                 =>
        FunctionApplication(target.map(substitute(_, paramName, argValue)), arg.map(substitute(_, paramName, argValue)))
      case FunctionLiteral(name, paramType, innerBody) if name != paramName =>
        FunctionLiteral(name, paramType, innerBody.map(substitute(_, paramName, argValue)))
      case _                                                                => body
    }

  /** Transform an expression by applying f to all children first, then to the result. */
  def transform(expr: ExpressionValue, f: ExpressionValue => ExpressionValue): ExpressionValue =
    f(expr match {
      case FunctionApplication(target, arg)       =>
        FunctionApplication(target.map(transform(_, f)), arg.map(transform(_, f)))
      case FunctionLiteral(name, paramType, body) =>
        FunctionLiteral(name, paramType, body.map(transform(_, f)))
      case leaf                                   => leaf
    })

  given Show[ExpressionValue] with {
    def show(expr: ExpressionValue): String = expr match {
      case FunctionType(paramType, returnType)    => s"Function(${paramType.show}, ${returnType.show})"
      case ConcreteValue(v)                       => v.show
      case FunctionLiteral(name, paramType, body) => s"(($name: ${paramType.show}) -> ${body.value.show})"
      case NativeFunction(paramType, _)           => s"native(${paramType.show})"
      case ParameterReference(name, _)            => name
      case FunctionApplication(target, arg)       => s"${target.value.show}(${arg.value.show})"
    }
  }

  val expressionValueUserDisplay: Show[ExpressionValue] = {
    case ConcreteValue(value)                   => Value.valueUserDisplay.show(value)
    case FunctionLiteral(name, paramType, body) =>
      s"($name: ${Value.valueUserDisplay.show(paramType)}) -> ${expressionValueUserDisplay.show(body.value)}"
    case NativeFunction(paramType, _)           => s"native(${Value.valueUserDisplay.show(paramType)})"
    case ParameterReference(name, _)            => name
    case FunctionApplication(target, arg)       =>
      s"${expressionValueUserDisplay.show(target.value)}(${expressionValueUserDisplay.show(arg.value)})"
  }

  def concreteValueOf(expressionValue: ExpressionValue): Option[Value] =
    expressionValue match {
      case ConcreteValue(value) => Some(value)
      case _                    => None
    }

  /** Strip leading FunctionLiteral wrappers that introduce type variables (parameterType == Value.Type). */
  @tailrec
  def stripUniversalTypeIntros(expr: ExpressionValue): ExpressionValue =
    expr match {
      case FunctionLiteral(_, Value.Type, body) => stripUniversalTypeIntros(body.value)
      case other                                => other
    }

  /** Match a pattern ExpressionValue (with type variable placeholders) against a concrete ExpressionValue, returning a
    * map from type variable names to their concrete bindings. Type variable names are ParameterReferences accepted by
    * the given predicate (default: all).
    */
  def matchTypes(
      pattern: ExpressionValue,
      concrete: ExpressionValue,
      isTypeVar: String => Boolean = _ => true
  ): Map[String, ExpressionValue] =
    (pattern, concrete) match {
      case (ParameterReference(name, _), _) if isTypeVar(name)                                        =>
        Map(name -> concrete)
      case (FunctionType(p1, r1), FunctionType(p2, r2))                                               =>
        matchTypes(p1, p2, isTypeVar) ++ matchTypes(r1, r2, isTypeVar)
      case (FunctionApplication(t1, a1), FunctionApplication(t2, a2))                                 =>
        matchTypes(t1.value, t2.value, isTypeVar) ++ matchTypes(a1.value, a2.value, isTypeVar)
      case (FunctionLiteral(_, _, patBody), FunctionLiteral(_, _, tgtBody))                           =>
        matchTypes(patBody.value, tgtBody.value, isTypeVar)
      case _                                                                                           => Map.empty
    }

  /** Create a function type: paramType -> returnType. Uses the standard Function^Type representation. */
  def functionType(paramType: ExpressionValue, returnType: ExpressionValue): ExpressionValue =
    FunctionApplication(unsourced(FunctionApplication(unsourced(Types.functionDataTypeExpr), unsourced(paramType))), unsourced(returnType))

  /** Extractor for function types. Matches FunctionApplication chains with Function^Type. */
  object FunctionType {
    def unapply(expr: ExpressionValue): Option[(ExpressionValue, ExpressionValue)] =
      expr match {
        case FunctionApplication(target, returnType)
            if isFunctionApplicationOfDataType(target.value) =>
          target.value match {
            case FunctionApplication(_, paramType) => Some((paramType.value, returnType.value))
            case _                                 => None
          }
        case _ => None
      }

    private def isFunctionApplicationOfDataType(expr: ExpressionValue): Boolean =
      expr match {
        case FunctionApplication(target, _) =>
          target.value match {
            case ConcreteValue(v) => isFunctionDataType(v)
            case _                => false
          }
        case _                              => false
      }

    private def isFunctionDataType(v: Value): Boolean =
      v match {
        case Value.Structure(fields, _) =>
          fields.get("$typeName") match {
            case Some(Value.Direct(vfqn: ValueFQN, _)) => vfqn === Types.functionDataTypeFQN
            case _                                     => false
          }
        case _                          => false
      }
  }

  sealed trait InitialExpressionValue extends ExpressionValue

  /** A concrete value of some type.
    */
  case class ConcreteValue(value: Value) extends InitialExpressionValue

  /** A function that "survived" the evaluation, i.e. there were no applications to evaluate it.
    */
  case class FunctionLiteral(
      parameterName: String,
      parameterType: Value,
      body: Sourced[ExpressionValue]
  ) extends InitialExpressionValue {
    override def equals(that: Any): Boolean = that match {
      case FunctionLiteral(n, t, b) => parameterName == n && parameterType == t && body.value == b.value
      case _                        => false
    }
    override def hashCode(): Int = (parameterName, parameterType, body.value).hashCode()
  }

  /** A native function that needs to be called with exact parameters.
    */
  case class NativeFunction(
      parameterType: Value,
      body: Value => ExpressionValue
  ) extends InitialExpressionValue

  /** A reference to a function parameter. Note: that this is only allowed somewhere in a function literal's body, not
    * on top level.
    */
  case class ParameterReference(parameterName: String, parameterType: Value) extends ExpressionValue

  /** An application of a function.
    */
  case class FunctionApplication(
      target: Sourced[ExpressionValue],
      argument: Sourced[ExpressionValue]
  ) extends ExpressionValue {
    override def equals(that: Any): Boolean = that match {
      case FunctionApplication(t, a) => target.value == t.value && argument.value == a.value
      case _                         => false
    }
    override def hashCode(): Int = (target.value, argument.value).hashCode()
  }
}
