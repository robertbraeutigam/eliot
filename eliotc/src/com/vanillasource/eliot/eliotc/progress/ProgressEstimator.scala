package com.vanillasource.eliot.eliotc.progress

import scala.concurrent.duration.*

/** How long a run has left, from what past runs of its class spent and what this one has done so far
  * (`docs/progress-indication.md` §3.4). A pure function, so it can be checked against recorded runs with no engine.
  *
  * The work of the `working` phase is weighed in historical time: each fact worked out counts as the average its key
  * type took in the past, and a fact still in flight counts as the time it has taken so far, up to that average — so a
  * single long fact keeps the estimate falling rather than holding it still. The work expected is the history's count
  * per key type — scaled to the total, since a history averaged over a program that grew counts too few — or what the
  * run has already reached where that is more. How fast this machine is today is the live time of the `working` phase
  * over the historical time of what it did, and it scales everything left, the timed phases included:
  * {{{
  * done      = Σ type  worked[type] × average[type]  +  Σ in flight  min(elapsed, average[type])
  * expected  = Σ type  max(history[type] × total / history facts, worked[type]) × average[type]
  * speed     = (working time + prior) / (done + prior)
  * remaining = (expected − done) × speed  +  Σ later timed phases × speed
  * }}}
  * `prior` is a tenth of the expected work taken as having run at the historical speed, which keeps the first few,
  * noisy facts from swinging the estimate. A key type the history does not know is weighed at its live time, which is
  * then both done and expected: a plugin nobody told the progress system about costs nothing to estimate.
  *
  * A run that has delivered more facts than the history's run has outgrown it, and the work it has left is unknown (the
  * graph is discovered by computing it, `docs/progress-indication.md` F5): it has no estimate until `working` is over,
  * rather than one reading `finishing` for as long as it keeps growing. The cache is saved in time proportional to the
  * facts, so the history's saving time is scaled the same way, by what the run delivered once `working` is over.
  */
object ProgressEstimator {

  /** How much of the expected work counts as having run at the historical speed before the run measures its own. */
  val priorWeight: Double = 0.1

  /** How long the run of `snapshot` has left, going by `history`, of a past run that delivered `total` facts; `None`
    * once it is [[ProgressPhase.Running]], which nothing estimates, and while it works past `total`.
    */
  def remaining(total: Long, history: ProgressHistory, snapshot: ProgressSnapshot): Option[FiniteDuration] =
    Option.when(
      snapshot.phase != ProgressPhase.Running &&
        !(snapshot.phase == ProgressPhase.Working && snapshot.delivered > total)
    ) {
      val delivered        = if (snapshot.phase.ordinal > ProgressPhase.Working.ordinal) snapshot.delivered else total
      val size             = if (history.facts > 0) delivered / history.facts else 1.0
      val (done, expected) = work(history, size, snapshot)
      val prior            = priorWeight * expected
      val workingTime      = nanosOf(snapshot.phaseTimes.getOrElse(ProgressPhase.Working, Duration.Zero))
      val speed            = if (done + prior > 0) (workingTime + prior) / (done + prior) else 1.0
      val workLeft         = (expected - done).max(0) * speed

      def timed(phase: ProgressPhase): Double =
        history.phases.getOrElse(phase, 0.0) * speed * (if (phase.ordinal > ProgressPhase.Working.ordinal) size
                                                        else 1.0)

      val current = snapshot.phase match {
        case ProgressPhase.Working => workLeft
        case phase                 =>
          (timed(phase) - nanosOf(snapshot.phaseTimes.getOrElse(phase, Duration.Zero))).max(0)
      }
      val later   = ProgressPhase.values
        .filter(phase => phase.ordinal > snapshot.phase.ordinal && phase != ProgressPhase.Running)
        .map(phase => if (phase == ProgressPhase.Working) workLeft else timed(phase))

      (current + later.sum).round.nanos
    }

  /** The work done and the work expected, in historical nanoseconds, of a run `size` times the history's. */
  private def work(history: ProgressHistory, size: Double, snapshot: ProgressSnapshot): (Double, Double) = {
    val inFlight = snapshot.inFlight.groupMap(_._1)(flying => nanosOf(flying._2))

    (history.types.keySet ++ snapshot.worked.keySet ++ inFlight.keySet).toSeq
      .map { name =>
        val worked = snapshot.worked.getOrElse(name, ProgressCost.zero)
        val flying = inFlight.getOrElse(name, Seq.empty)

        history.types.get(name).flatMap(past => past.average.map(past -> _)) match {
          case Some((past, average)) =>
            (
              worked.count * average + flying.map(_ min average).sum,
              (past.count * size max (worked.count + flying.size)) * average
            )
          case None                  =>
            val live = worked.nanos + flying.sum
            (live, live)
        }
      }
      .foldLeft((0.0, 0.0)) { case ((done, expected), (typeDone, typeExpected)) =>
        (done + typeDone, expected + typeExpected)
      }
  }

  private def nanosOf(duration: FiniteDuration): Double = duration.toNanos.toDouble
}
