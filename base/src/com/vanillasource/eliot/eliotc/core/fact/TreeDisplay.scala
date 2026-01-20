package com.vanillasource.eliot.eliotc.core.fact

/** A typeclass for pretty-printing tree-like structures such as expression stacks and their nested expressions.
  *
  * @tparam E
  *   The expression type (individual nodes that may contain nested stacks)
  */
trait TreeDisplay[E] {

  /** Render an expression node, returning its label and any named children (nested stacks) */
  def render(expression: E): TreeDisplay.Node[E]
}

object TreeDisplay {

  /** Represents a rendered expression node with a label and optional named children */
  case class Node[E](label: String, children: Seq[(String, ExpressionStack[E])] = Seq.empty)

  def prettyPrint[E](stack: ExpressionStack[E])(using TreeDisplay[E]): String = {
    val sb = StringBuilder()
    sb.append("ExpressionStack\n")
    prettyPrintLayers(stack, "", sb)
    sb.toString()
  }

  private def prettyPrintLayers[E](stack: ExpressionStack[E], prefix: String, sb: StringBuilder)(using
      printer: TreeDisplay[E]
  ): Unit = {
    stack.levelsWithLevel.foreach { case (expr, idx) =>
      val isLast   = idx == stack.maxLevel - 1
      val branch   = if (isLast) "└── " else "├── "
      val childPfx = if (isLast) "    " else "│   "
      sb.append(s"$prefix$branch[$idx] ")
      prettyPrintExpression(expr, prefix + childPfx, sb)
    }
  }

  private def prettyPrintExpression[E](expr: E, prefix: String, sb: StringBuilder)(using
      printer: TreeDisplay[E]
  ): Unit = {
    val node = printer.render(expr)
    sb.append(node.label)
    sb.append("\n")

    node.children.zipWithIndex.foreach { case ((childName, childStack), idx) =>
      val isLast   = idx == node.children.size - 1
      val branch   = if (isLast) "└── " else "├── "
      val childPfx = if (isLast) "    " else "│   "
      sb.append(s"$prefix$branch$childName:\n")
      prettyPrintNestedStack(childStack, prefix + childPfx, sb)
    }
  }

  private def prettyPrintNestedStack[E](stack: ExpressionStack[E], prefix: String, sb: StringBuilder)(using
      printer: TreeDisplay[E]
  ): Unit = {
    if (stack.expressions.isEmpty) {
      sb.append(s"$prefix└── (empty)\n")
    } else {
      prettyPrintLayers(stack, prefix, sb)
    }
  }
}
