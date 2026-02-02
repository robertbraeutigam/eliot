package com.vanillasource.eliot.eliotc.jvm.classgen.processor

import cats.data.StateT
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.uncurry2.fact.ParameterDefinition

case class TypeState(
    typeMap: Map[String, ParameterDefinition] = Map.empty,
    parameters: Seq[String] = Seq.empty,
    lambdaCount: Int = 0
)

object TypeState {
  type CompilationTypesIO[T] = StateT[CompilerIO, TypeState, T]

  extension [T](cio: CompilerIO[T]) {
    def liftToTypes: CompilationTypesIO[T] = StateT.liftF(cio)
  }

  def getParameterIndex(name: String): CompilationTypesIO[Option[Int]] =
    StateT.get[CompilerIO, TypeState].map(_.parameters.indexOf(name)).map(i => Option.when(i >= 0)(i))

  def addParameterDefinition(definition: ParameterDefinition): CompilationTypesIO[Unit] =
    StateT.modify[CompilerIO, TypeState] { state =>
      state.copy(
        typeMap = state.typeMap.updated(definition.name.value, definition),
        parameters = state.parameters.appended(definition.name.value)
      )
    }

  def incLambdaCount: CompilationTypesIO[Int] =
    StateT.modify[CompilerIO, TypeState] { state =>
      state.copy(lambdaCount = state.lambdaCount + 1)
    } >> StateT.get[CompilerIO, TypeState].map(_.lambdaCount)

  def getParameterType(name: String): CompilationTypesIO[Option[ParameterDefinition]] =
    StateT.get[CompilerIO, TypeState].map(_.typeMap.get(name))
}
