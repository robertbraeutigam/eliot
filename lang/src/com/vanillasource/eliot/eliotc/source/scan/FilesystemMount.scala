package com.vanillasource.eliot.eliotc.source.scan

import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.source.stat.FileStat

import java.io.File
import java.net.URI
import java.nio.file.Path

/** A filesystem source root: resolves a module-relative path to a `file:` URI when the file exists under `root`.
  * Existence is read through the [[FileStat]] fact (a leaf), so the scan's incremental invalidation hangs off the
  * file's stat exactly as before mounts existed.
  */
final case class FilesystemMount(root: Path) extends SourceMount {
  override def resolve(path: Path): CompilerIO[Option[URI]] =
    getFactOrAbort(FileStat.Key(root.resolve(path).toFile))
      .map(stat => Option.when(stat.lastModified.isDefined)(stat.file.toURI))

  /** Walks the whole tree under `root` and returns every `.els` file as a path relative to `root` (so it round-trips
    * through [[com.vanillasource.eliot.eliotc.module.fact.ModuleName.fromPath]]). Every directory visited is read
    * through its [[FileStat]] leaf first, recording a *listing* dependency on that directory's mtime: adding, removing
    * or renaming a file bumps its containing directory's mtime, which regenerates that `FileStat` and invalidates the
    * enclosing enumeration; an unchanged tree stays cached with no `readdir`. A nested subdirectory is itself a walked
    * directory, so its own `FileStat` covers files added below it. A missing `root` yields no paths (a `null`
    * `listFiles`), self-healing once it appears (its parent's mtime bump triggers the re-walk).
    */
  override def enumerate: CompilerIO[Seq[Path]] =
    walk(root.toFile).map(_.map(file => root.relativize(file.toPath)))

  private def walk(dir: File): CompilerIO[Seq[File]] =
    for {
      _        <- getFactOrAbort(FileStat.Key(dir)) // records a listing dependency on this directory's mtime
      children <- IO(Option(dir.listFiles()).map(_.toIndexedSeq).getOrElse(IndexedSeq.empty)).to[CompilerIO]
      elsFiles  = children.filter(file => file.isFile && file.getName.endsWith(".els"))
      nested   <- children.filter(_.isDirectory).toList.traverse(walk).map(_.flatten)
    } yield elsFiles ++ nested
}
