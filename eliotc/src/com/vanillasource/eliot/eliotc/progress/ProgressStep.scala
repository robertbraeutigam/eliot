package com.vanillasource.eliot.eliotc.progress

import scala.concurrent.duration.FiniteDuration

/** A described fact whose generation has finished, and how long the run spent on it — its own work and that of every
  * undescribed fact it asked for, but not the work of a described fact it asked for, which is a step of its own.
  */
case class ProgressStep(activity: ProgressActivity, took: FiniteDuration)
