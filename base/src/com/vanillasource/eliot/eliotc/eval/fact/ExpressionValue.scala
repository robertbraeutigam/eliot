package com.vanillasource.eliot.eliotc.eval.fact

import cats.Show
import cats.syntax.all.*

/** The result of an expression evaluation.
  */
sealed trait ExpressionValue

object ExpressionValue {

  /** Check if an expression contains a variable with the given name. Used for occurs check in unification. */
  def containsVar(expr: ExpressionValue, varName: String): Boolean =
    expr match {
      case ParameterReference(name, _)         => name == varName
      case FunctionApplication(target, arg)    => containsVar(target, varName) || containsVar(arg, varName)
      case FunctionLiteral(_, _, body)         => containsVar(body, varName)
      case ConcreteValue(_)                    => false
      case NativeFunction(_, _)                => false
    }

  /** Transform an expression by applying f to all children first, then to the result. */
  def transform(expr: ExpressionValue, f: ExpressionValue => ExpressionValue): ExpressionValue =
    f(expr match {
      case FunctionApplication(target, arg)       =>
        FunctionApplication(transform(target, f), transform(arg, f))
      case FunctionLiteral(name, paramType, body) =>
        FunctionLiteral(name, paramType, transform(body, f))
      case leaf => leaf
    })

  given Show[ExpressionValue] with {
    def show(expr: ExpressionValue): String = expr match {
      case FunctionType(paramType, returnType)      => s"(${show(paramType)}) -> ${show(returnType)}"
      case ConcreteValue(v)                         => v.toString
      case FunctionLiteral(name, paramType, body)   => s"($name: $paramType) -> ${show(body)}"
      case NativeFunction(paramType, _)             => s"native($paramType)"
      case ParameterReference(name, _)              => name
      case FunctionApplication(target, arg)         => s"${show(target)}(${show(arg)})"
    }
  }

  /** Marker for function types. Function types are represented as FunctionApplication chains. */
  val FunctionTypeMarker: ExpressionValue = ConcreteValue(Value.Structure(
    Map("$marker" -> Value.Direct("FunctionType", Value.Type)),
    Value.Type
  ))

  /** Create a function type: paramType -> returnType */
  def functionType(paramType: ExpressionValue, returnType: ExpressionValue): ExpressionValue =
    FunctionApplication(FunctionApplication(FunctionTypeMarker, paramType), returnType)

  /** Extractor for function types */
  object FunctionType {
    def unapply(expr: ExpressionValue): Option[(ExpressionValue, ExpressionValue)] =
      expr match {
        case FunctionApplication(FunctionApplication(marker, paramType), returnType)
            if marker == FunctionTypeMarker =>
          Some((paramType, returnType))
        case _ => None
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
      body: ExpressionValue
  ) extends InitialExpressionValue

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
      target: ExpressionValue,
      argument: ExpressionValue
  ) extends ExpressionValue
}
