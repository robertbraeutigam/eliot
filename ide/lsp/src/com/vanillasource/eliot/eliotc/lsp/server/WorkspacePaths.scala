package com.vanillasource.eliot.eliotc.lsp.server

import java.io.{IOException, UncheckedIOException}
import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

/** The source roots an `eliot.paths` file lists explicitly for the language server, split by pool.
  *
  * The server bundles no layers and knows no build system; the compiler itself is `javac`-like, fully parameterised by
  * `--path`. When there is no real build tool yet, a per-project `eliot.paths` file is the stopgap that tells the *LSP*
  * (only) where every source root is — the project's own code and the base/stdlib/platform layers it depends on — so the
  * server can hand them to the compiler as ordinary path parameters. Unlike [[SourceRootDiscovery]], nothing is guessed:
  * every root, including the project's own (there is no assumed `src/`), is listed. This keeps the file layout-agnostic —
  * a project may sit directly under `src/` with no `eliot/` qualifier and no `eliot-compiler/` sibling.
  *
  * The file lives in a workspace root and has one directive per line:
  * {{{
  *   # base + platform layers (from a compiler checkout, say), plus this project's own sources
  *   runtime  /home/me/eliot/lang/eliot
  *   runtime  /home/me/eliot/stdlib/eliot
  *   runtime  /home/me/eliot/jvm/eliot
  *   runtime  src
  *   # compile-time overlay roots, listed separately (the compiler pool scans these on top, override-preferred)
  *   compiler /home/me/eliot/stdlib/eliot-compiler
  * }}}
  * Blank lines and `#` comments are ignored; any other directive keyword is skipped. A path is everything after the
  * keyword, trimmed; a relative path resolves against the file's own directory (so `src` means `<project>/src`), an
  * absolute path is taken as-is. Both lists are normalised to absolute paths.
  *
  * @param runtimeRoots  the runtime pool roots (`LangPlugin.pathKey`) — project + layer `eliot/` roots.
  * @param compilerRoots the explicit compile-time overlay roots (`LangPlugin.compilerPathKey`).
  */
final case class WorkspacePaths(runtimeRoots: Seq[Path], compilerRoots: Seq[Path])

object WorkspacePaths {

  /** The per-project config file name, looked up in each workspace root. */
  val fileName = "eliot.paths"

  private val runtimeDirective  = "runtime"
  private val compilerDirective = "compiler"
  private val commentPrefix     = "#"

  /** Load and merge every `eliot.paths` found directly in the given workspace roots, or [[None]] if none has one. A
    * present file is *authoritative*: when this returns a value the caller uses exactly these roots and does not fall
    * back to [[SourceRootDiscovery]] — the whole point is that the roots are listed, not guessed.
    */
  def load(workspaceRoots: Seq[Path]): Option[WorkspacePaths] = {
    val parsed = workspaceRoots.flatMap(root => parse(root.resolve(fileName)))
    if (parsed.isEmpty) None
    else
      Some(
        WorkspacePaths(
          parsed.flatMap(_.runtimeRoots).distinct,
          parsed.flatMap(_.compilerRoots).distinct
        )
      )
  }

  private def parse(file: Path): Option[WorkspacePaths] =
    if (!Files.isRegularFile(file)) None
    else
      safely(None) {
        val directory = file.toAbsolutePath.getParent
        val entries   = Files
          .readAllLines(file)
          .asScala
          .map(_.trim)
          .filter(line => line.nonEmpty && !line.startsWith(commentPrefix))
          .flatMap(directiveOf(directory, _))
          .toSeq
        Some(
          WorkspacePaths(
            entries.collect { case (`runtimeDirective`, path) => path },
            entries.collect { case (`compilerDirective`, path) => path }
          )
        )
      }

  private def directiveOf(directory: Path, line: String): Option[(String, Path)] =
    line.split("\\s+", 2) match {
      case Array(keyword, rest) if rest.trim.nonEmpty =>
        Some(keyword -> directory.resolve(rest.trim).toAbsolutePath.normalize)
      case _                                          => None
    }

  private def safely[A](fallback: A)(body: => A): A =
    try body
    catch { case _: IOException | _: UncheckedIOException => fallback }
}
