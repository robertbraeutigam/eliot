package com.vanillasource.eliot.eliotc.resolve.processor

import cats.{Applicative, MonadError}
import cats.data.StateT
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.{CompilationProcess, CompilerProcessor, ast}
import com.vanillasource.eliot.eliotc.module.fact.{FunctionFQN, TypeFQN}
import com.vanillasource.eliot.eliotc.source.error.CompilationF.{CompilationF, compilationAbort, compilerAbort}

case class ResolverScope(
    functionDictionary: Map[String, FunctionFQN],
    typeDictionary: Map[String, TypeFQN],
    visibleGenericTypes: Map[String, ast.GenericParameter],
    visibleValues: Map[String, ast.ArgumentDefinition]
) {
  def isValueVisible(name: String): Boolean = visibleValues.contains(name)
}

object ResolverScope {
  type ScopedF[F[_], T] = StateT[[X] =>> CompilationF[F, X], ResolverScope, T]

  extension [F[_], T](io: CompilationF[F, T])(using MonadError[F, Throwable]) {
    def liftToScoped: ScopedF[F, T] = StateT.liftF[[X] =>> CompilationF[F, X], ResolverScope, T](io)
  }

  def addVisibleValue[F[_]](
      arg: ast.ArgumentDefinition
  )(using CompilationProcess[F], MonadError[F, Throwable]): ScopedF[F, Unit] =
    for {
      visibleValues <- StateT.get[[X] =>> CompilationF[F, X], ResolverScope].map(_.visibleValues)
      _             <- compilerAbort(arg.name.as("Name already exists in scope.")).liftToScoped
                         .whenA(visibleValues.contains(arg.name.value))
      _             <- StateT.modify[[X] =>> CompilationF[F, X], ResolverScope](
                         _.copy(visibleValues = visibleValues + (arg.name.value -> arg))
                       )
    } yield ()

  def isValueVisible[F[_]](name: String)(using MonadError[F, Throwable]): ScopedF[F, Boolean] = for {
    visibleValues <- StateT.get[[X] =>> CompilationF[F, X], ResolverScope].map(_.visibleValues)
  } yield visibleValues.contains(name)

  def getFunction[F[_]](name: String)(using MonadError[F, Throwable]): ScopedF[F, Option[FunctionFQN]] = for {
    functionDictionary <- StateT.get[[X] =>> CompilationF[F, X], ResolverScope].map(_.functionDictionary)
  } yield functionDictionary.get(name)

  def getType[F[_]](name: String)(using MonadError[F, Throwable]): ScopedF[F, Option[TypeFQN]] = for {
    typeDictionary <- StateT.get[[X] =>> CompilationF[F, X], ResolverScope].map(_.typeDictionary)
  } yield typeDictionary.get(name)

  def getGenericParameter[F[_]](
      name: String
  )(using MonadError[F, Throwable]): ScopedF[F, Option[ast.GenericParameter]] = for {
    genericParameters <- StateT.get[[X] =>> CompilationF[F, X], ResolverScope].map(_.visibleGenericTypes)
  } yield genericParameters.get(name)

}
