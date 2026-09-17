package com.vanillasource.eliot.eliotc.progress

/** The fixed phases of one compiler run, in the order they happen (`docs/progress-indication.md` §3.1). They are the
  * engine's, not a plugin's, so the list is the same whatever the pipeline turns out to be.
  *
  * Only [[Working]] has a measure of how far along it is (facts delivered); the others are timed only. [[Running]] is
  * the selected target executing what was built, which begins after the closing line is printed, so no progress line
  * ever names it.
  *
  * @param label
  *   how a progress line names the phase, e.g. `saving cache`
  */
enum ProgressPhase(val label: String) {

  /** Plugin discovery, configuration and the cache fingerprints. */
  case Starting extends ProgressPhase("starting")

  /** Reading the incremental cache from disk. */
  case LoadingCache extends ProgressPhase("loading cache")

  /** The fact engine at work: everything the target demands, generated or accepted from the cache. Deliberately not
    * "compiling": with a flashing backend most of it may be an upload.
    */
  case Working extends ProgressPhase("working")

  /** Building the next run's cache graph and writing it to disk. */
  case SavingCache extends ProgressPhase("saving cache")

  /** The target executing what the run produced (`run` mode). */
  case Running extends ProgressPhase("running")
}
