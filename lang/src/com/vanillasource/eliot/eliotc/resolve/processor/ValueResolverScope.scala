package com.vanillasource.eliot.eliotc.resolve.processor

import cats.data.StateT
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.module.fact.QualifiedName
import com.vanillasource.eliot.eliotc.module.fact.Qualifier.Ability
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, Role, ValueFQN}
import com.vanillasource.eliot.eliotc.platform.Platform
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.resolve.fact.AbilityFQN

case class ValueResolverScope(
    currentModule: ModuleName,
    dictionary: Map[QualifiedName, ValueFQN],
    privateNames: Map[QualifiedName, ValueFQN],
    parameters: Set[String],
    platform: Platform = Platform.Runtime
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

  def getPrivateName(name: QualifiedName): ScopedIO[Option[ValueFQN]] =
    StateT.get[CompilerIO, ValueResolverScope].map(_.privateNames.get(name))

  def searchAbilities(searchingValueName: String): ScopedIO[Seq[ValueFQN]] =
    StateT
      .get[CompilerIO, ValueResolverScope]
      .map(
        _.dictionary.values
          .collect {
            case vfqn @ ValueFQN(_, QualifiedName(valueName, Ability(_), Role.Runtime))
                if valueName === searchingValueName =>
              vfqn
          }
          .toSeq
      )

  /** Resolve an ability name exactly as any other name resolves: a keyed lookup of the ability's own **marker** —
    * the synthetic `Foo^Foo` value [[com.vanillasource.eliot.eliotc.ast.fact.AbilityBlock]] emits — in the ordinary
    * dictionary. An ability is a value, and this is the one lookup that says so
    * (`docs/effects-syntax-userspace.md` §4 stage 2).
    *
    * It replaces a scan of `dictionary.values` matching *any* member carrying the `Ability(name)` qualifier. That
    * scan resolved an ability by a mechanism no other name used, and got two things wrong: an ability resolved
    * whenever any *method* of it was in scope even if the weak prelude tier had dropped the marker itself, and with
    * two same-named abilities in scope the `collectFirst` winner was `Map` iteration order rather than the
    * dictionary's answer. Abilities are always public (`ability` takes no visibility modifier), so there is no
    * `privateNames` fallback to make here.
    */
  def getAbility(name: String): ScopedIO[Option[AbilityFQN]] =
    StateT
      .get[CompilerIO, ValueResolverScope]
      .map(_.dictionary.get(QualifiedName(name, Ability(name))).map(vfqn => AbilityFQN(vfqn.moduleName, name)))

  def getCurrentModule: ScopedIO[ModuleName] =
    StateT.get[CompilerIO, ValueResolverScope].map(_.currentModule)

  def getPlatform: ScopedIO[Platform] =
    StateT.get[CompilerIO, ValueResolverScope].map(_.platform)

  def withLocalScope[T](computation: ScopedIO[T]): ScopedIO[T] =
    for {
      originalScope <- StateT.get[CompilerIO, ValueResolverScope]
      result        <- computation
      _             <- StateT.set[CompilerIO, ValueResolverScope](originalScope)
    } yield result
}
