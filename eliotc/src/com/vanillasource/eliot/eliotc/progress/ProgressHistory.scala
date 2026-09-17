package com.vanillasource.eliot.eliotc.progress

/** Where the time of one class of run goes (`docs/progress-indication.md` §3.4): the wall time of each phase that has
  * no facts to count, and what the facts of each key type cost. A profile keeps one per [[ProgressRunClass]], each an
  * exponentially weighted average over the runs of that class, so a program that grows moves its history along with it.
  *
  * @param facts
  *   the facts the runs delivered, the unit its counts are in: a history averaged over a program that grew counts fewer
  *   facts of each type than the last run had, and the ratio of the two is by how much
  * @param phases
  *   the wall time of each timed phase ([[ProgressPhase.Starting]], [[ProgressPhase.LoadingCache]],
  *   [[ProgressPhase.SavingCache]]), in nanoseconds
  * @param types
  *   the cost of the facts worked out, by the key type's class name
  */
case class ProgressHistory(facts: Double, phases: Map[ProgressPhase, Double], types: Map[String, ProgressCost]) {

  /** This history with `run` folded in, `run` weighing [[ProgressHistory.newestWeight]]. A key type the run did not
    * have fades, and is forgotten once it averages less than half a fact.
    */
  def including(run: ProgressHistory): ProgressHistory = {
    val weight = ProgressHistory.newestWeight

    ProgressHistory(
      weight * run.facts + (1 - weight) * facts,
      (phases.keySet ++ run.phases.keySet)
        .map(phase =>
          phase -> (weight * run.phases.getOrElse(phase, 0.0) + (1 - weight) * phases.getOrElse(phase, 0.0))
        )
        .toMap,
      (types.keySet ++ run.types.keySet)
        .map(name =>
          name -> run.types.getOrElse(name, ProgressCost.zero).blend(weight, types.getOrElse(name, ProgressCost.zero))
        )
        .filter(_._2.count >= 0.5)
        .toMap
    )
  }
}

object ProgressHistory {
  val empty: ProgressHistory = ProgressHistory(0, Map.empty, Map.empty)

  /** How much the newest run weighs against all before it: half, so a history follows a changed program in a few runs.
    */
  val newestWeight: Double = 0.5

  /** The phases a history times; the others are either counted or begin after the run's last line. */
  val timedPhases: Seq[ProgressPhase] =
    Seq(ProgressPhase.Starting, ProgressPhase.LoadingCache, ProgressPhase.SavingCache)
}
