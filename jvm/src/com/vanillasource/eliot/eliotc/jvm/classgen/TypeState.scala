package com.vanillasource.eliot.eliotc.jvm.classgen

import cats.data.StateT
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.module.fact.TypeFQN
import com.vanillasource.eliot.eliotc.resolve.fact.ArgumentDefinition
import com.vanillasource.eliot.eliotc.source.error.CompilationIO.CompilationIO

case class TypeState(
    typeMap: Map[String, ArgumentDefinition] = Map.empty,
    parameters: Seq[String] = Seq.empty,
    lambdaCount: Int = 0
)

object TypeState {
  type CompilationTypesIO[T] = StateT[CompilationIO, TypeState, T]

  extension [T](cio: CompilationIO[T]) {
    def liftToTypes: CompilationTypesIO[T] = StateT.liftF(cio)
  }

  def getParameterIndex(name: String): CompilationTypesIO[Option[Int]] =
    StateT.get[CompilationIO, TypeState].map(_.parameters.indexOf(name)).map(i => Option.when(i >= 0)(i))

  def addParameterDefinition(definition: ArgumentDefinition): CompilationTypesIO[Unit] =
    StateT.modify[CompilationIO, TypeState] { state =>
      state.copy(
        typeMap = state.typeMap.updated(definition.name.value, definition),
        parameters = state.parameters.appended(definition.name.value)
      )
    }

  def incLambdaCount: CompilationTypesIO[Int] =
    StateT.modify[CompilationIO, TypeState] { state =>
      state.copy(lambdaCount = state.lambdaCount + 1)
    } >> StateT.get[CompilationIO, TypeState].map(_.lambdaCount)

  def getParameterType(name: String): CompilationTypesIO[Option[ArgumentDefinition]] =
    StateT.get[CompilationIO, TypeState].map(_.typeMap.get(name))
}
