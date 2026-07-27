package com.vanillasource.eliot.eliotc.lsp.mainroot

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, QualifiedName, Qualifier, ValueFQN}
import com.vanillasource.eliot.eliotc.pos.PositionRange
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.source.content.{SourceContent, Sourced}
import com.vanillasource.eliot.eliotc.processor.common.SingleKeyTypeProcessor

import java.net.URI
import java.nio.file.Path

/** Serves the sources of the LSP's synthesized per-module monomorphization roots (the `lspmain.*` wrapper modules
  * mounted by [[LspMainRootMount]]).
  *
  * The idiomatic user `main` is carrier-generic (`def main: {Console} Unit` desugars to an inferable carrier
  * `F[Unit]`), so it cannot be a monomorphization root by itself — the type-hint driver roots at a wrapper that binds
  * the carrier exactly as the jvm target's synthesized entry point does
  * (`jvm`'s `SyntheticMainSourceProcessor`): `block`'s expected type instantiates the carrier to `eliot.jvm.IO`,
  * resolved from the workspace's jvm layer on the path. A workspace without that layer simply fails to monomorphize
  * the wrapper, and hints degrade exactly as for any failing `main`. Contents are a pure function of the wrapped
  * module's name, so the facts are ordinary generated leaves.
  */
class LspMainRootSourceProcessor extends SingleKeyTypeProcessor[SourceContent.Key] {
  import LspMainRootSourceProcessor.*

  override protected def generateFact(key: SourceContent.Key): CompilerIO[Unit] =
    if (key.uri.getScheme != scheme) ().pure[CompilerIO]
    else
      targetModuleOf(key.uri) match {
        case Some(target) =>
          registerFactIfClear(SourceContent(key.uri, Sourced(key.uri, PositionRange.zero, wrapperSource(target))))
        case None         => ().pure[CompilerIO]
      }
}

object LspMainRootSourceProcessor {

  /** The URI scheme namespace this processor owns. */
  val scheme: String = "lsp-main"

  /** The reserved module package the wrapper modules live under: module `M` is wrapped by module `lspmain.M`. A user
    * source tree that also declares a module under `lspmain` collides in the ordinary layer merge, loudly.
    */
  val reservedPackage: String = "lspmain"

  /** The wrapper module wrapping `target`. */
  def wrapperModule(target: ModuleName): ModuleName = ModuleName(reservedPackage +: target.packages, target.name)

  /** The `main` of the wrapper module wrapping `target` — the value the type-hint driver roots monomorphization at. */
  def wrapperVfqn(target: ModuleName): ValueFQN =
    ValueFQN(wrapperModule(target), QualifiedName("main", Qualifier.Default))

  /** Whether a module-relative scan path names a wrapper module (`lspmain/.../M.els`). */
  def isWrapperPath(path: Path): Boolean =
    path.getNameCount >= 2 && path.getName(0).toString == reservedPackage && path.toString.endsWith(".els")

  /** The URI a wrapper scan path resolves to (separator-normalized, so the mapping is OS-independent). */
  def uriFor(path: Path): URI =
    URI.create(s"$scheme:${(0 until path.getNameCount).map(path.getName(_).toString).mkString("/")}")

  /** The module a wrapper URI wraps, when the URI is well-formed (`lsp-main:lspmain/.../M.els`). */
  def targetModuleOf(uri: URI): Option[ModuleName] = {
    val wrapper = ModuleName.fromPath(Path.of(uri.getSchemeSpecificPart))
    Option.when(wrapper.packages.headOption.contains(reservedPackage))(
      ModuleName(wrapper.packages.tail, wrapper.name)
    )
  }

  /** The wrapper names the platform run boundary `runMain` rather than inlining its `apply(block(…), unit)` body, for
    * the same reason the jvm target's synthesized entry does ([[runBoundaryVfqn]]): the boundary is what tells the
    * compiler this call *captures* the wrapped `main`'s whole effect row. Inlined, the wrapper reads as an ordinary
    * definition returning `Unit` that performs the user's effects without declaring them, and the per-definition row
    * verification rejects it — correctly, since nothing in that spelling says a carrier is being run.
    */
  private def wrapperSource(target: ModuleName): String =
    s"""
       |import eliot.jvm.IO
       |def main: Unit = runMain(${target.show}::main)
       |""".stripMargin

  /** The platform run boundary [[wrapperSource]] calls (`def runMain[A](io: IO[A]): A`, in the workspace's jvm layer).
    * Registered by [[com.vanillasource.eliot.eliotc.lsp.plugin.LspPlugin]] exactly as the jvm plugin registers it, so
    * the wrapper's capture is recognised here as it is in a real build. Spelled by name, like the `import eliot.jvm.IO`
    * the wrapper already carries: the LSP does not depend on the jvm module.
    */
  val runBoundaryVfqn: ValueFQN =
    ValueFQN(ModuleName(Seq("eliot", "jvm"), "IO"), QualifiedName("runMain", Qualifier.Default))
}
