package com.vanillasource.eliot.eliotc.eval.fact

import cats.Show
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN

import scala.annotation.tailrec

/** The result of an expression evaluation.
  */
sealed trait ExpressionValue

object ExpressionValue {

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
        onFunApp(fold(onConcrete, onNative, onParamRef, onFunApp, onFunLit)(target),
                 fold(onConcrete, onNative, onParamRef, onFunApp, onFunLit)(arg))
      case FunctionLiteral(name, paramType, body) =>
        onFunLit(name, paramType, fold(onConcrete, onNative, onParamRef, onFunApp, onFunLit)(body))
    }

  /** Check if an expression contains a variable with the given name. Used for occurs check in unification. */
  def containsVar(expr: ExpressionValue, varName: String): Boolean =
    fold(_ => false, _ => false, (name, _) => name == varName, _ || _, (_, _, inBody) => inBody)(expr)

  /** Strip all leading FunctionLiteral wrappers, returning the innermost body. */
  @tailrec
  def stripLeadingLambdas(expr: ExpressionValue): ExpressionValue =
    expr match {
      case FunctionLiteral(_, _, body) => stripLeadingLambdas(body)
      case other                       => other
    }

  @tailrec
  def stripLeadingFunctionApplications(expr: ExpressionValue): ExpressionValue =
    expr match {
      case FunctionApplication(target, _) => stripLeadingFunctionApplications(target)
      case other                          => other
    }

  /** Extract parameter names and types from leading FunctionLiteral wrappers. */
  def extractLeadingLambdaParams(expr: ExpressionValue): Seq[(String, Value)] =
    expr match {
      case FunctionLiteral(name, paramType, body) => (name, paramType) +: extractLeadingLambdaParams(body)
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
        FunctionApplication(substitute(target, paramName, argValue), substitute(arg, paramName, argValue))
      case FunctionLiteral(name, paramType, innerBody) if name != paramName =>
        FunctionLiteral(name, paramType, substitute(innerBody, paramName, argValue))
      case _                                                                => body
    }

  /** Transform an expression by applying f to all children first, then to the result. */
  def transform(expr: ExpressionValue, f: ExpressionValue => ExpressionValue): ExpressionValue =
    f(expr match {
      case FunctionApplication(target, arg)       =>
        FunctionApplication(transform(target, f), transform(arg, f))
      case FunctionLiteral(name, paramType, body) =>
        FunctionLiteral(name, paramType, transform(body, f))
      case leaf                                   => leaf
    })

  given Show[ExpressionValue] with {
    def show(expr: ExpressionValue): String = expr match {
      case FunctionType(paramType, returnType)    => s"Function(${paramType.show}, ${returnType.show})"
      case ConcreteValue(v)                       => v.show
      case FunctionLiteral(name, paramType, body) => s"(($name: ${paramType.show}) -> ${body.show})"
      case NativeFunction(paramType, _)           => s"native(${paramType.show})"
      case ParameterReference(name, _)            => name
      case FunctionApplication(target, arg)       => s"${target.show}(${arg.show})"
    }
  }

  val expressionValueUserDisplay: Show[ExpressionValue] = {
    case ConcreteValue(value)                   => Value.valueUserDisplay.show(value)
    case FunctionLiteral(name, paramType, body) =>
      s"($name: ${Value.valueUserDisplay.show(paramType)}) -> ${expressionValueUserDisplay.show(body)}"
    case NativeFunction(paramType, _)           => s"native(${Value.valueUserDisplay.show(paramType)})"
    case ParameterReference(name, _)            => name
    case FunctionApplication(target, arg)       =>
      s"${expressionValueUserDisplay.show(target)}(${expressionValueUserDisplay.show(arg)})"
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
      case FunctionLiteral(_, Value.Type, body) => stripUniversalTypeIntros(body)
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
        matchTypes(t1, t2, isTypeVar) ++ matchTypes(a1, a2, isTypeVar)
      case (FunctionLiteral(_, _, patBody), FunctionLiteral(_, _, tgtBody))                           =>
        matchTypes(patBody, tgtBody, isTypeVar)
      case _                                                                                           => Map.empty
    }

  /** Create a function type: paramType -> returnType. Uses the standard Function^Type representation. */
  def functionType(paramType: ExpressionValue, returnType: ExpressionValue): ExpressionValue =
    FunctionApplication(FunctionApplication(Types.functionDataTypeExpr, paramType), returnType)

  /** Extractor for function types. Matches FunctionApplication chains with Function^Type. */
  object FunctionType {
    def unapply(expr: ExpressionValue): Option[(ExpressionValue, ExpressionValue)] =
      expr match {
        case FunctionApplication(FunctionApplication(ConcreteValue(v), paramType), returnType)
            if isFunctionDataType(v) =>
          Some((paramType, returnType))
        case _ => None
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
