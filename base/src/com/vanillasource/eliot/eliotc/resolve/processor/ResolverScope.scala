package com.vanillasource.eliot.eliotc.resolve.processor

import cats.data.StateT
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.module.fact.{FunctionFQN, TypeFQN}
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.ast
import com.vanillasource.eliot.eliotc.ast.fact.{ArgumentDefinition, GenericParameter}
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerError

case class ResolverScope(
                          functionDictionary: Map[String, FunctionFQN],
                          typeDictionary: Map[String, TypeFQN],
                          visibleGenericTypes: Map[String, GenericParameter],
                          visibleValues: Map[String, ArgumentDefinition]
) {
  def isValueVisible(name: String): Boolean = visibleValues.contains(name)
}

object ResolverScope {
  type ScopedIO[T] = StateT[CompilerIO, ResolverScope, T]

  extension [T](io: CompilerIO[T]) {
    def liftToScoped: ScopedIO[T] = StateT.liftF[CompilerIO, ResolverScope, T](io)
  }

  def addVisibleValue(arg: ArgumentDefinition): ScopedIO[Unit] = for {
    visibleValues <- StateT.get[CompilerIO, ResolverScope].map(_.visibleValues)
    _             <- (compilerError(arg.name.as("Name already exists in scope.")) *> abort[Unit]).liftToScoped
                       .whenA(visibleValues.contains(arg.name.value))
    _             <- StateT.modify[CompilerIO, ResolverScope](
                       _.copy(visibleValues = visibleValues + (arg.name.value -> arg))
                     )
  } yield ()

  def isValueVisible(name: String): ScopedIO[Boolean] = for {
    visibleValues <- StateT.get[CompilerIO, ResolverScope].map(_.visibleValues)
  } yield visibleValues.contains(name)

  def getFunction(name: String): ScopedIO[Option[FunctionFQN]] = for {
    functionDictionary <- StateT.get[CompilerIO, ResolverScope].map(_.functionDictionary)
  } yield functionDictionary.get(name)

  def getType(name: String): ScopedIO[Option[TypeFQN]] = for {
    typeDictionary <- StateT.get[CompilerIO, ResolverScope].map(_.typeDictionary)
  } yield typeDictionary.get(name)

  def getGenericParameter(name: String): ScopedIO[Option[GenericParameter]] = for {
    genericParameters <- StateT.get[CompilerIO, ResolverScope].map(_.visibleGenericTypes)
  } yield genericParameters.get(name)

}
