package com.vanillasource.eliot.eliotc.apidoc.plugin

import cats.data.StateT
import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.apidoc.build.DocModelBuilder
import com.vanillasource.eliot.eliotc.apidoc.render.HtmlSite
import com.vanillasource.eliot.eliotc.ast.fact.{AST, SourceAST}
import com.vanillasource.eliot.eliotc.compiler.Compiler
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.module.fact.ModuleName
import com.vanillasource.eliot.eliotc.plugin.Configuration.namedKey
import com.vanillasource.eliot.eliotc.plugin.LangPlugin.{compilerPathKey, pathKey, runtimePathKey}
import com.vanillasource.eliot.eliotc.plugin.{CompilerPlugin, Configuration, LangPlugin}
import com.vanillasource.eliot.eliotc.processor.{CompilationProcess, CompilerProcessor}
import com.vanillasource.eliot.eliotc.stdlib.plugin.StdlibPlugin
import scopt.{OParser, OParserBuilder}

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

/** The compilation target for documentation generation: a whole-workspace apidoc-site driver.
  *
  * Like the language server it has no `main` to drive from, so it enumerates the filesystem source roots itself and, for
  * every `.els` file, demands [[SourceAST]] (which forces tokenize → parse, attaching the captured `/** ... */` doc
  * comments to each declaration) and stops there — it never resolves, type-checks, or generates code. Because it scans
  * the abstract base, the platform layers, and the user program together, the generated site documents them as one,
  * merging the abstract signature of a name with the layers that implement it.
  *
  * It adds no processors of its own; the front end comes from its plugin dependencies. `JvmPlugin` is deliberately not a
  * dependency (and is kept off the classpath by the build module), so no codegen processor is ever activated.
  */
class ApiDocPlugin extends CompilerPlugin with Logging {
  private val cmdLineBuilder: OParserBuilder[Configuration] = OParser.builder[Configuration]
  import cmdLineBuilder.*

  override def commandLineParser(): OParser[?, Configuration] = OParser.sequence(
    cmd("apidoc")
      .text("generate an HTML documentation site for every .els under the source roots")
      .action((_, config) => config.set(ApiDocPlugin.selectedKey, true))
  )

  override def isSelectedBy(configuration: Configuration): Boolean = configuration.contains(ApiDocPlugin.selectedKey)

  override def pluginDependencies(configuration: Configuration): Seq[Class[? <: CompilerPlugin]] = Seq(
    classOf[LangPlugin],
    classOf[StdlibPlugin]
  )

  /** Adds no processors of its own — the whole front end (source readers, tokenizer, parser) comes from [[LangPlugin]].
    */
  override def initialize(configuration: Configuration): StateT[IO, CompilerProcessor, Unit] = StateT.empty

  override def run(configuration: Configuration, compilation: CompilationProcess): IO[Unit] =
    for {
      layerFiles <- collectLayerFiles(configuration, compilation)
      _          <- debug[IO](s"Apidoc collected ${layerFiles.size} source file(s) for documentation.")
      built       = DocModelBuilder.build(layerFiles)
      _          <- built.warnings.traverse_(warn[IO](_))
      outDir      = configuration.getOrElse(Compiler.targetPathKey, Path.of("target")).resolve("apidoc")
      _          <- writeSite(outDir, HtmlSite.render(built.modules))
      _          <- info[IO](s"Generated documentation for ${built.modules.size} module(s): ${outDir.resolve("index.html")}.")
    } yield ()

  /** Read every `.els` under every (distinct) source root, returning each parsed file tagged with its module name and
    * the layer (the root directory it was found under) so downstream merging can attribute each declaration to a layer.
    */
  private def collectLayerFiles(
      configuration: Configuration,
      compilation: CompilationProcess
  ): IO[Seq[(ModuleName, String, AST)]] =
    rootsWithLayer(configuration).flatTraverse { case (root, layer) =>
      eliotFilesUnder(root).flatMap(_.flatTraverse { file =>
        compilation.getFact(SourceAST.Key(file.toFile.toURI)).map {
          case Some(sourceAst) => Seq((moduleNameOf(root, file), layer, sourceAst.ast.value))
          case None            => Seq.empty
        }
      })
    }

  /** The distinct source roots to document, each paired with a human-readable layer label. The base appears in both the
    * compiler and runtime path; deduplicating by absolute path keeps it as one root. The user's program paths fold in as
    * additional roots.
    */
  private def rootsWithLayer(configuration: Configuration): Seq[(Path, String)] = {
    val roots =
      configuration.getOrElse(compilerPathKey, Seq.empty) ++
        configuration.getOrElse(runtimePathKey, Seq.empty) ++
        configuration.getOrElse(pathKey, Seq.empty)
    roots.map(_.toAbsolutePath.normalize).distinct.map(root => root -> layerLabel(root))
  }

  private def layerLabel(root: Path): String = {
    val fileName = root.getFileName.toString
    if (fileName == "eliot") Option(root.getParent).map(_.getFileName.toString).getOrElse(fileName) else fileName
  }

  private def eliotFilesUnder(root: Path): IO[Seq[Path]] = IO.blocking {
    if (Files.isDirectory(root)) {
      val walk = Files.walk(root)
      try walk.iterator().asScala.filter(p => Files.isRegularFile(p) && isEliotSource(p)).toVector
      finally walk.close()
    } else if (Files.isRegularFile(root) && isEliotSource(root)) {
      Seq(root)
    } else {
      Seq.empty
    }
  }

  private def moduleNameOf(root: Path, file: Path): ModuleName = {
    val relative = root.relativize(file)
    val segments = (0 until relative.getNameCount).map(relative.getName(_).toString).toVector
    ModuleName(segments.dropRight(1), segments.last.dropRight(ApiDocPlugin.eliotExtension.length))
  }

  private def isEliotSource(path: Path): Boolean = path.getFileName.toString.endsWith(ApiDocPlugin.eliotExtension)

  private def writeSite(outDir: Path, files: Seq[(String, String)]): IO[Unit] = IO.blocking {
    Files.createDirectories(outDir)
    files.foreach { case (relativePath, content) => Files.writeString(outDir.resolve(relativePath), content) }
  }
}

object ApiDocPlugin {
  private val selectedKey: Configuration.Key[Boolean] = namedKey[Boolean]("apidocSelected")
  private val eliotExtension                          = ".els"
}
