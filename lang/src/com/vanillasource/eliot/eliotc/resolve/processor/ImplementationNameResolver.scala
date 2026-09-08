package com.vanillasource.eliot.eliotc.resolve.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.module.fact.{ModuleAbilities, ValueFQN}
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.resolve.processor.ValueResolverScope.*
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerAbort

/** Resolves the `h` of a `with h` to the implementation it names — effects v6's resolution step (`docs/effects.md` §9.4
  * step 1). The answer is the implementation's **marker**
  * ([[com.vanillasource.eliot.eliotc.monomorphize.check.ImplementationBinding]]'s head), which is the only thing that
  * flows onward: a name is never carried downstream to be searched by, because such a name would ignore import scope,
  * could not be shadowed, and would be decided by hash order (§12).
  *
  * Two keyed steps, no scan: first the implementation's **name marker** in the ordinary dictionary
  * ([[ValueResolverScope.getImplementation]]), which is what makes the lookup honour imports and shadowing exactly as
  * an ability name's does — and which names the module the implementation lives in; then that module's
  * [[ModuleAbilities.markerOfImplementationName]], which is the implementation's real marker.
  *
  * The second step exists because a marker's qualified name also carries the ability name and the pattern key, and
  * `with h` supplies neither. A name that resolves to no implementation is a hard error at the `with` — never a silent
  * fall-back to the default, which would make a mistyped fake silently run the real thing.
  */
object ImplementationNameResolver {

  def resolve(name: Sourced[String]): ScopedIO[ValueFQN] =
    getImplementation(name.value).flatMap {
      case None             => compilerAbort[ValueFQN](name.as("Implementation not found.")).liftToScoped
      case Some(nameMarker) =>
        for {
          platform  <- getPlatform
          abilities <- getFactOrError(ModuleAbilities.Key(nameMarker.moduleName, platform))(
                         Sourced.compilerError(name.as("Implementation not found."))
                       ).liftToScoped
          marker    <- abilities.markerOfImplementationName(name.value) match {
                         // The name marker and the implementation are minted together, so a name marker with no
                         // implementation beside it is a compiler defect, not a user error — but it is reported at the
                         // `with` rather than thrown, so a broken module cannot take the build down at an unrelated
                         // position.
                         case Some(marker) => marker.pure[ScopedIO]
                         case None         =>
                           compilerAbort[ValueFQN](
                             name.as("Implementation has no marker.")
                           ).liftToScoped
                       }
        } yield marker
    }
}
