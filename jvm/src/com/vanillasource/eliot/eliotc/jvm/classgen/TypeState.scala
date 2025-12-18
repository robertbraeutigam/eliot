package com.vanillasource.eliot.eliotc.jvm.classgen

import cats.data.StateT
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.resolve.fact.ArgumentDefinition
import com.vanillasource.eliot.eliotc.source.error.CompilationIO.CompilationIO

case class TypeState(typeMap: Map[String, ArgumentDefinition] = Map.empty, lambdaCount: Int = 0)

object TypeState {
  type CompilationTypesIO[T] = StateT[CompilationIO, TypeState, T]

  extension [T](cio: CompilationIO[T]) {
    def liftToTypes: CompilationTypesIO[T] = StateT.liftF(cio)
  }

  def addParameterDefinition(definition: ArgumentDefinition): CompilationTypesIO[Unit] =
    StateT.modify[CompilationIO, TypeState] { state =>
      state.copy(typeMap = state.typeMap.updated(definition.name.value, definition))
    }

  def incLambdaCount: CompilationTypesIO[Int] =
    StateT.modify[CompilationIO, TypeState] { state =>
      state.copy(lambdaCount = state.lambdaCount + 1)
    } >> StateT.get[CompilationIO, TypeState].map(_.lambdaCount)

  def getParameterTypeMap: CompilationTypesIO[Map[String, ArgumentDefinition]] =
    StateT.get[CompilationIO, TypeState].map(_.typeMap)
}
