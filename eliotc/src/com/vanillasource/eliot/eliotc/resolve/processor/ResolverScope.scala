package com.vanillasource.eliot.eliotc.resolve.processor

import cats.data.StateT
import com.vanillasource.eliot.eliotc.ast
import com.vanillasource.eliot.eliotc.module.{FunctionFQN, TypeFQN}
import com.vanillasource.eliot.eliotc.source.CompilationIO.CompilationIO

case class ResolverScope(
    functionDictionary: Map[String, FunctionFQN],
    typeDictionary: Map[String, TypeFQN],
    visibleGenericTypes: Map[String, ast.GenericParameter],
    visibleValues: Map[String, ast.ArgumentDefinition]
) {
  def isValueVisible(name: String): Boolean = visibleValues.contains(name)
}

object ResolverScope {
  type ScopedIO[T] = StateT[CompilationIO, ResolverScope, T]

  extension [T](io: CompilationIO[T]) {
    def liftToScoped: ScopedIO[T] = StateT.liftF[CompilationIO, ResolverScope, T](io)
  }
}
