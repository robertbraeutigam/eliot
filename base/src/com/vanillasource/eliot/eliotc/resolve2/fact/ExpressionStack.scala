package com.vanillasource.eliot.eliotc.resolve2.fact

import cats.syntax.all.*
import cats.Show
import com.vanillasource.eliot.eliotc.resolve2.fact.Expression.*

/** An expression stack's "bottom" describes a runtime value (like a constant, a function, or even a type), and each
  * layer above describes the underlying layer's "type". A single constant value like: "zero: Int = 0" will have 2
  * layers, with the first having a "0" as literal, and the layer over it being a reference to the "Int" value (which
  * can be considered a "type"). Each layer can refer to any value declared on the same level syntactically "before" the
  * usage site, or to any value on layers above.
  */
case class ExpressionStack(expressions: Seq[Expression])

object ExpressionStack {
  def of(expression: Expression) = ExpressionStack(Seq(expression))

  def empty = ExpressionStack(Seq.empty)

  given Show[ExpressionStack] = (stack: ExpressionStack) => stack.expressions.map(_.show).mkString(" => ")

  def prettyPrint(stack: ExpressionStack): String = {
    val sb = StringBuilder()
    sb.append("ExpressionStack\n")
    prettyPrintLayers(stack.expressions, "", sb)
    sb.toString()
  }

  private def prettyPrintLayers(expressions: Seq[Expression], prefix: String, sb: StringBuilder): Unit = {
    expressions.zipWithIndex.foreach { case (expr, idx) =>
      val isLast   = idx == expressions.size - 1
      val branch   = if (isLast) "└── " else "├── "
      val childPfx = if (isLast) "    " else "│   "
      sb.append(s"$prefix$branch[$idx] ")
      prettyPrintExpression(expr, prefix + childPfx, sb)
    }
  }

  private def prettyPrintExpression(expr: Expression, prefix: String, sb: StringBuilder): Unit = {
    expr match {
      case IntegerLiteral(sourced)  =>
        sb.append(s"IntegerLiteral(${sourced.value})\n")
      case StringLiteral(sourced)   =>
        sb.append(s"StringLiteral(\"${sourced.value}\")\n")
      case ParameterReference(name) =>
        sb.append(s"ParameterReference(${name.value})\n")
      case ValueReference(name)     =>
        sb.append(s"ValueReference(${name.value.show})\n")
      case FunctionApplication(target, argument) =>
        sb.append("FunctionApplication\n")
        sb.append(s"$prefix├── target:\n")
        prettyPrintNestedStack(target.value, prefix + "│   ", sb)
        sb.append(s"$prefix└── argument:\n")
        prettyPrintNestedStack(argument.value, prefix + "    ", sb)
      case FunctionLiteral(param, paramType, body) =>
        sb.append(s"FunctionLiteral(${param.value})\n")
        sb.append(s"$prefix├── parameterType:\n")
        prettyPrintNestedStack(paramType.value, prefix + "│   ", sb)
        sb.append(s"$prefix└── body:\n")
        prettyPrintNestedStack(body.value, prefix + "    ", sb)
    }
  }

  private def prettyPrintNestedStack(stack: ExpressionStack, prefix: String, sb: StringBuilder): Unit = {
    if (stack.expressions.isEmpty) {
      sb.append(s"$prefix└── (empty)\n")
    } else {
      prettyPrintLayers(stack.expressions, prefix, sb)
    }
  }
}
