package com.vanillasource.eliot.eliotc.progress

import com.vanillasource.eliot.eliotc.processor.CompilerFactKey

import java.net.URI
import java.nio.file.Path
import scala.util.Try

/** How a plugin's facts read to a user (`docs/progress-indication.md` §3.5). Partial by design: a key nobody describes is
  * still counted, and its time is shown under the nearest described fact that asked for it.
  *
  * A plugin describes the key types it owns, e.g.
  * {{{
  * key => key match {
  *   case SourceTokens.Key(uri) => Some(ProgressActivity("parsing", ProgressDescriber.uriSubject(uri)))
  *   case _                     => None
  * }
  * }}}
  */
trait ProgressDescriber {

  /** The activity generating `key`'s fact is, or `None` when this describer does not know the key. */
  def describe(key: CompilerFactKey[?]): Option[ProgressActivity]
}

object ProgressDescriber {

  /** Describes nothing. */
  val none: ProgressDescriber = _ => None

  /** The first description any of `describers` gives. */
  def combined(describers: Seq[ProgressDescriber]): ProgressDescriber =
    key => describers.iterator.map(_.describe(key)).collectFirst { case Some(activity) => activity }

  /** A file as a subject: relative to the working directory when it is under it, e.g. `examples/src/HelloWorld.els`. */
  def fileSubject(file: Path): String = {
    val absolute = file.toAbsolutePath.normalize()
    val here     = Path.of("").toAbsolutePath

    (if (absolute.startsWith(here)) here.relativize(absolute) else absolute).toString
  }

  /** A source URI as a subject: a `file:` URI as its file ([[fileSubject]]), any other the part after its scheme
    * (`jvm-main:main.els` reads `main.els`).
    */
  def uriSubject(uri: URI): String =
    if (uri.getScheme == "file") Try(fileSubject(Path.of(uri))).getOrElse(uri.toString)
    else Option(uri.getSchemeSpecificPart).getOrElse(uri.toString)
}
