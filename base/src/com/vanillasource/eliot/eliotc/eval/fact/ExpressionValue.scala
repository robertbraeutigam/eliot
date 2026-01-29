package com.vanillasource.eliot.eliotc.eval.fact

/** The result of an expression evaluation.
  */
sealed trait ExpressionValue

object ExpressionValue {
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
