package com.vanillasource.eliot.eliotc.progress

import java.util.Locale

/** One measure of what a run produced — the number an embedded developer came for (`docs/progress-indication.md`
  * §4.4): `HelloWorld.jar 412 KB` today, `flash 2.14 KB / 256 KB` on a microcontroller target. The target plugin supplies
  * them ([[com.vanillasource.eliot.eliotc.plugin.CompilerPlugin.progressMeasures]]); the progress profile keeps the
  * last value of each, so the closing line can say how much it moved.
  *
  * @param name
  *   what is measured, unique among a target's measures
  * @param value
  *   how much of it there is
  * @param quantity
  *   what the value counts
  * @param limit
  *   how much there may be, if anything bounds it
  */
case class ProgressMeasure(
    name: String,
    value: Long,
    quantity: ProgressMeasure.Quantity,
    limit: Option[Long] = None
) {

  /** The measure as shown, e.g. `flash 2.14 KB / 256 KB (+12 B)`, with the change since the `previous` value, if there
    * was one. A value over its limit is shown as a failure.
    */
  def show(previous: Option[Long], style: ProgressStyle): String = {
    val amount = quantity.show(value)
    val shown  = if (limit.exists(value > _)) style.fail(amount) else amount
    val bound  = limit.fold("")(limit => s" / ${quantity.show(limit)}")
    val delta  = previous.fold("")(before => style.faint(s" (${quantity.showChange(value - before)})"))

    s"$name $shown$bound$delta"
  }
}

object ProgressMeasure {

  /** What a measure's value counts. */
  enum Quantity {

    /** A size in bytes, shown in binary units: `640 B`, `2.14 KB`, `412 KB`, `1.30 MB`. */
    case Bytes

    /** A number of things, shown as it is: `1,204`. */
    case Count

    def show(value: Long): String =
      this match {
        case Bytes => bytes(value)
        case Count => "%,d".formatLocal(Locale.ROOT, value)
      }

    /** A change of `delta`, always signed: `+88 B`, `-1.20 KB`, `+0`. */
    def showChange(delta: Long): String = (if (delta < 0) "-" else "+") + show(delta.abs)
  }

  private val units = Seq("KB", "MB", "GB", "TB")

  private def bytes(value: Long): String =
    if (value < 1024) s"$value B"
    else {
      val (amount, unit) = units.tail.foldLeft((value / 1024.0, units.head)) { case ((amount, unit), next) =>
        if (amount >= 1024) (amount / 1024, next) else (amount, unit)
      }
      val digits         = if (amount < 10) 2 else if (amount < 100) 1 else 0

      s"%.${digits}f %s".formatLocal(Locale.ROOT, amount, unit)
    }
}
