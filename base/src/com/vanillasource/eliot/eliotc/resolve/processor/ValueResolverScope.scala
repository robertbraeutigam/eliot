package com.vanillasource.eliot.eliotc.resolve.processor

import cats.data.StateT
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.core.fact.{QualifiedName, Qualifier}
import com.vanillasource.eliot.eliotc.core.fact.Qualifier.Ability
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*

case class ValueResolverScope(
    dictionary: Map[QualifiedName, ValueFQN],
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

  def getValue(name: QualifiedName): ScopedIO[Option[ValueFQN]] =
    StateT.get[CompilerIO, ValueResolverScope].map(_.dictionary.get(name))

  def searchAbilities(searchingName: String): ScopedIO[Seq[ValueFQN]] =
    StateT
      .get[CompilerIO, ValueResolverScope]
      .map(
        _.dictionary.values
          .collect {
            case vfqn @ ValueFQN(_, QualifiedName(valueName, Ability(_))) if valueName === searchingName => vfqn
          }
          .toSeq
      )

  def withLocalScope[T](computation: ScopedIO[T]): ScopedIO[T] =
    for {
      originalScope <- StateT.get[CompilerIO, ValueResolverScope]
      result        <- computation
      _             <- StateT.set[CompilerIO, ValueResolverScope](originalScope)
    } yield result
}
