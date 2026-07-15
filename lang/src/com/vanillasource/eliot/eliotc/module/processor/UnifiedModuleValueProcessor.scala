package com.vanillasource.eliot.eliotc.module.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.core.fact.NamedValue
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.module.fact.{ModuleNames, ModuleValue, Role, UnifiedModuleValue, ValueFQN}
import com.vanillasource.eliot.eliotc.platform.Platform
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.SingleFactProcessor
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.source.content.Sourced.{compilerError, compilerAbort}
import com.vanillasource.eliot.eliotc.source.scan.PathScan

import java.net.URI

class UnifiedModuleValueProcessor extends SingleFactProcessor[UnifiedModuleValue.Key] with Logging {

  override protected def generateSingleFact(key: UnifiedModuleValue.Key): CompilerIO[UnifiedModuleValue] =
    for {
      pathScan     <- getFactOrAbort(PathScan.Key(key.vfqn.moduleName.toPath, key.platform))
      allNames     <- pathScan.files.traverse(file => getFactOrAbort(ModuleNames.Key(file)).map(file -> _))
      // A value is located by its `Runtime`-role *surface* name (the only role in [[ModuleNames]]); a `Signature`-twin
      // request lives in exactly the files where its runtime twin is declared. The `ModuleValue` demand below then uses
      // the full role-bearing `vfqn`, so the signature twin's own facts are fetched.
      surfaceName   = key.vfqn.name.copy(role = Role.Runtime)
      filesWithName = allNames.collect { case (file, names) if names.names.value.contains(surfaceName) => file }
      allValues    <- filesWithName.traverse(file => getFactOrAbort(ModuleValue.Key(file, key.vfqn, key.platform)))
      _            <- compilerAbort(allNames.head._2.names.as(s"Could not find '${key.vfqn.name.show}'."))
                        .whenA(allValues.isEmpty)
      unifiedValue <- unifyValues(key.vfqn, allValues, pathScan.overrideFiles, key.platform)
    } yield unifiedValue

  /** Reconciles every co-located declaration of a name across the pool's layers into one value. The merge is **per
    * `(name, role)`** — the role is part of `vfqn`, so a given call sees only one role's declarations:
    *
    *   - **`Runtime` twins** follow the prefer-the-implementation rule below.
    *   - **`Signature` twins** are always bodied (a declaration always has a signature) and must all *agree* — there is
    *     no "prefer the implementation" / "multiple implementations" notion, since every layer simply restates the same
    *     signature. Agreement is `hasSameSignatures` (a signature twin's `signature` slot repeats its body, so this
    *     compares the signature expressions); the override overlay may still supersede. The `dischargedEffects` union
    *     stays on the runtime twin for now (the effect phase reads it there), so the signature merge carries none.
    *
    * The runtime rules stay order-free with one exception, the compiler-as-platform override: among the
    * *implementations* (`runtime.isDefined`), an implementation that comes from an **override file** (the compiler
    * overlay, per [[PathScan.overrideFiles]]) supersedes the platform's — so the compiler track can redefine a name the
    * platform also implements without it counting as a conflict. Absent any override (always so on the runtime track,
    * where `overrideFiles` is empty), the ordinary rule applies: more than one implementation is a hard error. Every
    * declaration must still agree on signature.
    */
  private def unifyValues(
      vfqn: ValueFQN,
      values: Seq[ModuleValue],
      overrideFiles: Set[URI],
      platform: Platform
  ): CompilerIO[UnifiedModuleValue] =
    if (values.isEmpty) {
      abort
    } else if (!hasSameSignatures(values)) {
      compilerError(values.head.namedValue.qualifiedName.as("Has multiple different definitions.")) *> abort
    } else if (vfqn.name.role == Role.Signature) {
      val overriding = values.filter(value => overrideFiles.contains(value.uri))
      val chosen     = overriding.headOption.getOrElse(values.head)
      UnifiedModuleValue(chosen.vfqn, chosen.dictionary, chosen.namedValue, chosen.privateNames, platform)
        .pure[CompilerIO]
    } else {
      val implementations = values.filter(_.namedValue.runtime.isDefined)
      val overriding      = implementations.filter(value => overrideFiles.contains(value.uri))
      val effective       = if (overriding.nonEmpty) overriding else implementations

      if (effective.sizeIs > 1) {
        compilerError(values.head.namedValue.qualifiedName.as("Has multiple implementations.")) *> abort
      } else {
        val chosen = effective.headOption.getOrElse(values.head)

        unifyDischargedEffects(values).flatMap { discharged =>
          logOverride(vfqn, overriding, implementations) *>
            UnifiedModuleValue(
              chosen.vfqn,
              chosen.dictionary,
              chosen.namedValue.copy(dischargedEffects = discharged),
              chosen.privateNames,
              platform
            ).pure[CompilerIO]
        }
      }
    }

  /** Unify the discharge annotation (the negative `{…, -E}` members) across all co-located declarations of a name. It
    * is a signature-level property but a generated `data`-field accessor cannot spell it, so it is kept out of
    * `signatureEquality`: the annotated abstract declaration's discharge simply rides onto the chosen (concrete)
    * implementation. Two layers carrying *different, non-empty* discharge sets is a real disagreement and is rejected.
    */
  private def unifyDischargedEffects(values: Seq[ModuleValue]): CompilerIO[Seq[Sourced[String]]] = {
    val nonEmpty = values.map(_.namedValue.dischargedEffects).filter(_.nonEmpty)
    if (nonEmpty.map(_.map(_.value).toSet).distinct.sizeIs > 1)
      compilerError(values.head.namedValue.qualifiedName.as("Layers disagree on the discharged effects.")) *> abort
    else nonEmpty.headOption.getOrElse(Seq.empty).pure[CompilerIO]
  }

  /** Trace an *actual* override — an overlay implementation superseding at least one platform implementation. */
  private def logOverride(
      vfqn: ValueFQN,
      overriding: Seq[ModuleValue],
      implementations: Seq[ModuleValue]
  ): CompilerIO[Unit] =
    if (overriding.nonEmpty && implementations.sizeIs > overriding.size)
      debug[CompilerIO](
        s"OVERRIDE ${vfqn.show}: overlay ${overriding.map(_.uri).mkString(", ")} supersedes " +
          implementations.filterNot(overriding.contains).map(_.uri).mkString(", ")
      )
    else ().pure[CompilerIO]

  private def hasSameSignatures(values: Seq[ModuleValue]): Boolean = {
    val first = values.head

    values.tail.forall(v => NamedValue.signatureEquality.eqv(first.namedValue, v.namedValue))
  }
}
