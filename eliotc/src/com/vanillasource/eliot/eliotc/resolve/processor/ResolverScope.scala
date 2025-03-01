package com.vanillasource.eliot.eliotc.resolve.processor

import com.vanillasource.eliot.eliotc.ast
import com.vanillasource.eliot.eliotc.module.{FunctionFQN, TypeFQN}

case class ResolverScope(
    functionDictionary: Map[String, FunctionFQN],
    typeDictionary: Map[String, TypeFQN],
    visibleGenericTypes: Map[String, ast.GenericParameter],
    visibleValues: Map[String, ast.ArgumentDefinition]
) {
  def isValueVisible(name: String): Boolean = visibleValues.contains(name)
}
