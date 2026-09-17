package com.vanillasource.eliot.eliotc.progress

import cats.effect.IO
import com.vanillasource.eliot.eliotc.feedback.Logging

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, NoSuchFileException, Path, StandardCopyOption}
import scala.util.Try

/** What the previous `--progress` run of a configuration learned about itself, which is what this run is measured
  * against (`docs/progress-indication.md` §3.2).
  *
  * It lives in the **profile file** `<target>/.eliot-progress-<configuration fingerprint>`, beside the incremental
  * cache but not discarded with it, so the cold build after a compiler upgrade still has its total. The file is text,
  * one record per line:
  * {{{
  * total 27181
  * cold facts 27181.0
  * cold phase LoadingCache 612000000
  * cold type com.vanillasource.eliot.eliotc.token.SourceTokens$Key 91.5 402000000
  * }}}
  * A line this compiler does not know is ignored, so a later profile can add to it without an older compiler failing.
  *
  * @param total
  *   the facts the previous successful run delivered; absent before the first one
  * @param runs
  *   where the time of each class of run went, averaged over the successful runs of that class (§3.4)
  */
case class ProgressProfile(total: Option[Long], runs: Map[ProgressRunClass, ProgressHistory] = Map.empty) {

  /** The profile after a successful run of class `runClass` that delivered `delivered` facts and spent its time as
    * `run` says.
    */
  def including(delivered: Long, runClass: ProgressRunClass, run: ProgressHistory): ProgressProfile =
    ProgressProfile(Some(delivered), runs.updated(runClass, runs.get(runClass).fold(run)(_.including(run))))

  /** The file's text. */
  def render: String = {
    val histories = for {
      runClass <- ProgressRunClass.values.toSeq
      history  <- runs.get(runClass).toSeq
      record   <- Seq(s"facts ${history.facts}") ++
                    history.phases.toSeq.sortBy(_._1.ordinal).map((phase, nanos) => s"phase $phase ${nanos.round}") ++
                    history.types.toSeq.sortBy(_._1).map((name, cost) => s"type $name ${cost.count} ${cost.nanos.round}")
    } yield s"${runClass.label} $record"

    (total.map(count => s"total $count").toSeq ++ histories).map(_ + "\n").mkString
  }
}

object ProgressProfile extends Logging {
  val empty: ProgressProfile = ProgressProfile(None)

  /** The profile file of the configuration whose fingerprint is `configFingerprint`, under `targetPath`. The name holds
    * a truncated, sanitized fingerprint, the way the cache files' names do.
    */
  def fileIn(targetPath: Path, configFingerprint: String): Path =
    targetPath.resolve(s".eliot-progress-${configFingerprint.filter(_.isLetterOrDigit).take(16)}")

  /** Read a file's text. Fail-safe: a line that does not parse is skipped, and so a damaged file costs at most the
    * total.
    */
  def parse(text: String): ProgressProfile = {
    val records = text.linesIterator.map(_.trim.split("\\s+").toSeq).toSeq
    val total   = records.collectFirst { case Seq("total", count) if natural(count).isDefined => count.toLong }
    val entries = records.flatMap {
      case Seq(runClass, "facts", facts)             =>
        for {
          c <- ProgressRunClass.fromLabel(runClass)
          f <- facts.toDoubleOption.filter(_ >= 0)
        } yield c -> ((history: ProgressHistory) => history.copy(facts = f))
      case Seq(runClass, "phase", phase, nanos)      =>
        for {
          c <- ProgressRunClass.fromLabel(runClass)
          p <- Try(ProgressPhase.valueOf(phase)).toOption
          n <- natural(nanos)
        } yield c -> ((history: ProgressHistory) => history.copy(phases = history.phases.updated(p, n.toDouble)))
      case Seq(runClass, "type", name, count, nanos) =>
        for {
          c <- ProgressRunClass.fromLabel(runClass)
          k <- count.toDoubleOption.filter(_ >= 0)
          n <- natural(nanos)
        } yield c -> ((history: ProgressHistory) =>
          history.copy(types = history.types.updated(name, ProgressCost(k, n.toDouble)))
        )
      case _                                         => None
    }
    val runs    = entries
      .groupMap(_._1)(_._2)
      .view
      .mapValues(_.foldLeft(ProgressHistory.empty)((history, record) => record(history)))

    ProgressProfile(total, runs.toMap)
  }

  private def natural(text: String): Option[Long] = text.toLongOption.filter(_ >= 0)

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
    }.handleErrorWith(t => warn[IO]("Could not write the progress profile; the next run will show the count alone.", t))
}
