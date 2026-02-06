package com.vanillasource.eliot.eliotc.resolve.processor

import cats.data.StateT
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*

case class ValueResolverScope(
    dictionary: Map[String, ValueFQN],
    parameters: Set[String]
)

object ValueResolverScope {
  type ScopedIO[T] = StateT[CompilerIO, ValueResolverScope, T]

  extension [T](io: CompilerIO[T]) {
    def liftToScoped: ScopedIO[T] = StateT.liftF[CompilerIO, ValueResolverScope, T](io)
  }

  def addParameter(name: String): ScopedIO[Unit] =
    StateT.modify[CompilerIO, ValueResolverScope](s => s.copy(parameters = s.parameters + name))

  def isParameter(name: String): ScopedIO[Boolean] =
    StateT.get[CompilerIO, ValueResolverScope].map(_.parameters.contains(name))

  def getValue(name: String): ScopedIO[Option[ValueFQN]] =
    StateT.get[CompilerIO, ValueResolverScope].map(_.dictionary.get(name))

  def withLocalScope[T](computation: ScopedIO[T]): ScopedIO[T] =
    for {
      originalScope <- StateT.get[CompilerIO, ValueResolverScope]
      result        <- computation
      _             <- StateT.set[CompilerIO, ValueResolverScope](originalScope)
    } yield result
}
