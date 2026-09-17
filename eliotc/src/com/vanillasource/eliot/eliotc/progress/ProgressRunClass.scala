package com.vanillasource.eliot.eliotc.progress

/** What kind of run this is, as far as how long it takes goes (`docs/progress-indication.md` §3.4). Runs of one class
  * repeat closely, runs of different classes do not, so each class keeps a history of its own.
  *
  * A run starts as [[Cold]] or [[Unchanged]], depending on whether there was a cache to load, and an unchanged run
  * becomes [[Changed]] the moment the cache's validation finds a fact that recomputes differently.
  *
  * @param label
  *   how the profile file names the class, e.g. `cold`
  */
enum ProgressRunClass(val label: String) {

  /** No cache: every fact is generated. */
  case Cold extends ProgressRunClass("cold")

  /** A cache, and something the program is built from came out different from it. */
  case Changed extends ProgressRunClass("changed")

  /** A cache, and nothing found different from it so far. */
  case Unchanged extends ProgressRunClass("unchanged")
}

object ProgressRunClass {

  /** The class the profile file names `label`. */
  def fromLabel(label: String): Option[ProgressRunClass] = values.find(_.label == label)
}
