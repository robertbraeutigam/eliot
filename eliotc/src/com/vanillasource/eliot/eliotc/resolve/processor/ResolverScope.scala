package com.vanillasource.eliot.eliotc.resolve.processor

import cats.data.StateT
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.{CompilationProcess, CompilerProcessor, ast}
import com.vanillasource.eliot.eliotc.module.{FunctionFQN, TypeFQN}
import com.vanillasource.eliot.eliotc.source.CompilationIO.{CompilationIO, compilationAbort, compilerAbort}

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

  def addVisibleValue(arg: ast.ArgumentDefinition)(using CompilationProcess): ScopedIO[Unit] = for {
    visibleValues <- StateT.get[CompilationIO, ResolverScope].map(_.visibleValues)
    _             <- compilerAbort(arg.name.as("Name already exists in scope.")).liftToScoped
                       .whenA(visibleValues.contains(arg.name.value))
    _             <- StateT.modify[CompilationIO, ResolverScope](
                       _.copy(visibleValues = visibleValues + (arg.name.value -> arg))
                     )
  } yield ()

  def isValueVisible(name: String): ScopedIO[Boolean] = for {
    visibleValues <- StateT.get[CompilationIO, ResolverScope].map(_.visibleValues)
  } yield visibleValues.contains(name)

  def getFunction(name: String): ScopedIO[Option[FunctionFQN]] = for {
    functionDictionary <- StateT.get[CompilationIO, ResolverScope].map(_.functionDictionary)
  } yield functionDictionary.get(name)

  def getType(name: String): ScopedIO[Option[TypeFQN]] = for {
    typeDictionary <- StateT.get[CompilationIO, ResolverScope].map(_.typeDictionary)
  } yield typeDictionary.get(name)

  def getGenericParameter(name: String): ScopedIO[Option[ast.GenericParameter]] = for {
    genericParameters <- StateT.get[CompilationIO, ResolverScope].map(_.visibleGenericTypes)
  } yield genericParameters.get(name)

}
