package com.vanillasource.eliot.eliotc.core.fact

import com.vanillasource.eliot.eliotc.core.fact.ExpressionStack.ExpressionStack

/** The core AST unifies data and functions into "names values". I.e. everything is a value, even types and functions.
  * Values can be "ad-hoc" (i.e. lambda expressions, i.e function literals), or named. Named values are those values
  * that have a dedicated name, so that they can be referred to in other expression. Note, that the expression stack is
  * split here because a named value can be "abstract" in which case it doesn't actually declare a value expression.
  * @param typeStack
  *   The expression stack representing the layers above the runtime value.
  * @param value
  *   The runtime value expression. This is None if the named value is abstract.
  */
case class NamedValue(typeStack: ExpressionStack, value: Option[Expression])
