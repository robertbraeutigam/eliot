package com.vanillasource.eliot.eliotc.lsp.plugin

import cats.data.StateT
import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.lsp.virtual.{
  VirtualFileContentReader,
  VirtualFileStatProcessor,
  VirtualFileSystem
}
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, UnifiedModuleNames, ValueFQN}
import com.vanillasource.eliot.eliotc.plugin.{CompilerPlugin, Configuration, LangPlugin}
import com.vanillasource.eliot.eliotc.processor.{CompilationProcess, CompilerProcessor}
import com.vanillasource.eliot.eliotc.processor.common.SequentialCompilerProcessors
import com.vanillasource.eliot.eliotc.saturate.fact.SaturatedValue
import com.vanillasource.eliot.eliotc.stdlib.plugin.StdlibPlugin

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

/** The compilation target for the language server: a whole-workspace diagnostics driver.
  *
  * A batch build is driven *from `main`* — [[com.vanillasource.eliot.eliotc.used.UsedNamesProcessor]] walks the
  * reachable monomorphized graph, so only code reachable from `main` is ever demanded. An editor has no `main` and must
  * surface diagnostics for **every** name in the workspace, including unreferenced and generic definitions. This driver
  * therefore enumerates the filesystem source roots itself and, for every `(ModuleName, QualifiedName)` it finds,
  * demands [[SaturatedValue]] — which transitively forces tokenize → parse → core → resolve → matchdesugar → operator
  * and surfaces the bulk of diagnostics **without** use-site instantiation. Deeper type errors inside generic bodies
  * stay use-site-verified per the language cornerstone (they surface where the value is instantiated), not at the
  * definition; widening the driver toward those is future work.
  *
  * For the front end it adds no processors of its own (that comes from [[LangPlugin]]); the one exception is the
  * virtual file system — [[VirtualFileStatProcessor]] + [[VirtualFileContentReader]] — which it inserts *ahead* of the
  * on-disk source readers so unsaved editor buffers are type-checked in place of stale disk content. Otherwise it is
  * purely the `run` target. The errors it triggers accumulate in the generator and come back via
  * [[com.vanillasource.eliot.eliotc.compiler.CompilationResult.errors]].
  */
class LspPlugin(vfs: VirtualFileSystem) extends CompilerPlugin with Logging {

  override def pluginDependencies(configuration: Configuration): Seq[Class[? <: CompilerPlugin]] = Seq(
    classOf[LangPlugin],
    classOf[StdlibPlugin]
  )

  /** Insert the virtual-file-system overlay ahead of everything else. Because [[LspPlugin]] is the first activated
    * plugin, the processor it leaves here becomes the *innermost-first* of the assembled chain, so its `FileStat` /
    * `FileContent` overrides register before the on-disk readers
    * ([[com.vanillasource.eliot.eliotc.source.stat.FileStatProcessor]] /
    * [[com.vanillasource.eliot.eliotc.source.file.FileContentReader]]) added by [[LangPlugin]], and win by being first.
    */
  override def initialize(configuration: Configuration): StateT[IO, CompilerProcessor, Unit] =
    StateT.modify(superProcessor =>
      SequentialCompilerProcessors(
        Seq(superProcessor, VirtualFileStatProcessor(vfs), VirtualFileContentReader(vfs))
      )
    )

  override def run(configuration: Configuration, compilation: CompilationProcess): IO[Unit] =
    for {
      modules <- workspaceModules(configuration.getOrElse(LangPlugin.pathKey, Seq.empty))
      _       <- debug[IO](s"LSP checking ${modules.size} workspace module(s): ${modules.map(_.show).mkString(", ")}")
      _       <- modules.traverse_(checkModule(compilation, _))
    } yield ()

  /** Demand a front-end fact for every name the module declares, so each one's diagnostics are produced. */
  private def checkModule(compilation: CompilationProcess, moduleName: ModuleName): IO[Unit] =
    compilation.getFact(UnifiedModuleNames.Key(moduleName)).flatMap {
      case None        => IO.unit // unification itself failed; its error is already reported
      case Some(names) =>
        names.names.keys.toList.traverse_(qn => compilation.getFact(SaturatedValue.Key(ValueFQN(moduleName, qn))).void)
    }

  /** Walk the filesystem source roots for `.els` files and derive each one's module name from its path relative to the
    * root it was found under. Classpath resources (the bundled stdlib) are intentionally excluded — the editor
    * diagnoses the user's workspace, not its dependencies.
    */
  private def workspaceModules(roots: Seq[Path]): IO[Seq[ModuleName]] =
    roots.flatTraverse(modulesUnder).map(_.distinct)

  private def modulesUnder(root: Path): IO[Seq[ModuleName]] = IO.blocking {
    if (Files.isRegularFile(root) && isEliotSource(root)) {
      Seq(moduleNameOf(root.getParent, root))
    } else if (Files.isDirectory(root)) {
      Files
        .walk(root)
        .iterator()
        .asScala
        .filter(p => Files.isRegularFile(p) && isEliotSource(p))
        .map(p => moduleNameOf(root, p))
        .toSeq
    } else {
      Seq.empty
    }
  }

  /** A module name is its file's path relative to the source root, with `.els` stripped: `pkg/Sub/Foo.els` under root
    * `pkg` ⇒ `ModuleName(["Sub"], "Foo")`, matching the forward `packages/name.els` mapping used by `PathScanner`.
    */
  private def moduleNameOf(base: Path, file: Path): ModuleName = {
    val relative = base.relativize(file)
    val segments = (0 until relative.getNameCount).map(relative.getName(_).toString).toVector
    ModuleName(segments.dropRight(1), stripEliotExtension(segments.last))
  }

  private def isEliotSource(path: Path): Boolean = path.getFileName.toString.endsWith(LspPlugin.eliotExtension)

  private def stripEliotExtension(fileName: String): String = fileName.dropRight(LspPlugin.eliotExtension.length)
}

object LspPlugin {
  private val eliotExtension = ".els"
}
