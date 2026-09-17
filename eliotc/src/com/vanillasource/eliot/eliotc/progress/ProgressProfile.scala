package com.vanillasource.eliot.eliotc.progress

import cats.effect.IO
import com.vanillasource.eliot.eliotc.feedback.Logging

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, NoSuchFileException, Path, StandardCopyOption}
import scala.util.Try

/** What the previous `--progress` run of a configuration learned about itself, which is what this run is measured
  * against (`docs/progress-indication.md` §3.2).
  *
  * It lives in the **profile file** `<target>/.eliot-progress-<configuration fingerprint>`, beside the incremental cache
  * but not discarded with it, so the cold build after a compiler upgrade still has its total. The file is text, one
  * `name value` pair per line:
  * {{{
  * total 27181
  * }}}
  * A line this compiler does not know is ignored, so a later profile can add to it without an older compiler failing.
  *
  * @param total
  *   the facts the previous successful run delivered; absent before the first one
  */
case class ProgressProfile(total: Option[Long]) {

  /** The file's text. */
  def render: String = total.fold("")(count => s"total $count\n")
}

object ProgressProfile extends Logging {
  val empty: ProgressProfile = ProgressProfile(None)

  /** The profile file of the configuration whose fingerprint is `configFingerprint`, under `targetPath`. The name holds
    * a truncated, sanitized fingerprint, the way the cache files' names do.
    */
  def fileIn(targetPath: Path, configFingerprint: String): Path =
    targetPath.resolve(s".eliot-progress-${configFingerprint.filter(_.isLetterOrDigit).take(16)}")

  /** Read a file's text. Fail-safe: a line that does not parse is skipped, and so a damaged file costs at most the total.
    */
  def parse(text: String): ProgressProfile =
    ProgressProfile(
      text.linesIterator
        .map(_.trim.split("\\s+"))
        .collectFirst { case Array("total", count) if Try(count.toLong).toOption.exists(_ >= 0) => count.toLong }
    )

  /** Read the profile from `file`; a missing or unreadable file is a profile with nothing in it. */
  def read(file: Path): IO[ProgressProfile] =
    IO.blocking(parse(Files.readString(file, StandardCharsets.UTF_8)))
      .handleErrorWith {
        case _: NoSuchFileException => IO.pure(empty)
        case t                      => warn[IO]("Could not read the progress profile; showing the count alone.", t).as(empty)
      }

  /** Write `profile` to `file`, replacing it in one step, so a concurrent reader sees the old profile or the new one.
    * Fail-safe: a profile that cannot be written costs the next run its total, never this run its result.
    */
  def write(file: Path, profile: ProgressProfile): IO[Unit] =
    IO.blocking {
      Files.createDirectories(file.getParent)
      val temporary = Files.createTempFile(file.getParent, file.getFileName.toString, ".tmp")
      try {
        Files.writeString(temporary, profile.render, StandardCharsets.UTF_8)
        Files.move(temporary, file, StandardCopyOption.REPLACE_EXISTING, StandardCopyOption.ATOMIC_MOVE): Unit
      } finally Files.deleteIfExists(temporary): Unit
    }
      .handleErrorWith(t => warn[IO]("Could not write the progress profile; the next run will show the count alone.", t))
}
