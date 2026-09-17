package com.vanillasource.eliot.eliotc.progress

/** How many facts of one key type a run worked out, and the time they took together — each one's own time, not that of
  * the facts it asked for. Fractional, because a history is an average over runs.
  *
  * @param count
  *   facts worked out, generated or accepted from the cache
  * @param nanos
  *   the time they took, in nanoseconds
  */
case class ProgressCost(count: Double, nanos: Double) {

  /** What one of these facts takes, or `None` if there is nothing to average over. */
  def average: Option[Double] = Option.when(count > 0)(nanos / count)

  /** `this` weighted by `weight`, plus `other` weighted by the rest. */
  def blend(weight: Double, other: ProgressCost): ProgressCost =
    ProgressCost(weight * count + (1 - weight) * other.count, weight * nanos + (1 - weight) * other.nanos)
}

object ProgressCost {
  val zero: ProgressCost = ProgressCost(0, 0)
}
