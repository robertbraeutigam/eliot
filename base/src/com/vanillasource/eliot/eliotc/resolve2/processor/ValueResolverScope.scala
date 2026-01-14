package com.vanillasource.eliot.eliotc.resolve2.processor

import cats.data.StateT
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.module2.fact.ValueFQN
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerError

case class ValueResolverScope(
    dictionary: Map[String, ValueFQN],
    visibleParameters: Map[String, Sourced[String]]
)

object ValueResolverScope {
  type ScopedIO[T] = StateT[CompilerIO, ValueResolverScope, T]

  extension [T](io: CompilerIO[T]) {
    def liftToScoped: ScopedIO[T] = StateT.liftF[CompilerIO, ValueResolverScope, T](io)
  }

  def addParameter(name: Sourced[String]): ScopedIO[Unit] =
    for {
      scope <- StateT.get[CompilerIO, ValueResolverScope]
      _     <- (compilerError(name.as("Parameter shadows existing name in scope.")) *> abort[Unit]).liftToScoped
                 .whenA(scope.visibleParameters.contains(name.value) || scope.dictionary.contains(name.value))
      _     <- StateT.modify[CompilerIO, ValueResolverScope](s =>
                 s.copy(visibleParameters = s.visibleParameters + (name.value -> name))
               )
    } yield ()

  def isParameter(name: String): ScopedIO[Boolean] =
    StateT.get[CompilerIO, ValueResolverScope].map(_.visibleParameters.contains(name))

  def getValue(name: String): ScopedIO[Option[ValueFQN]] =
    StateT.get[CompilerIO, ValueResolverScope].map(_.dictionary.get(name))
}
