package com.vanillasource.eliot.eliotc.progress

/** What a fact's generation is, in the words a user recognises: a verb and a subject, e.g. `checking eliot.lang.String`
  * or `reading examples/src/HelloWorld.els` (`docs/progress-indication.md` §3.5). The subject is a module or a file,
  * never a single value: values change thousands of times a second, modules a few times.
  *
  * The subject is worked out only when it is read. Every generation of a run is described, and only a handful of
  * descriptions are ever printed, so building every subject up front — a normalised path, a joined module name — is
  * work a cold build pays 27,000 times for nothing.
  *
  * @param verb
  *   what is being done, lowercase, e.g. `parsing`
  * @param input
  *   whether the fact *is* one of the program's inputs — a source file's content, the artefact on disk — so that a
  *   change to it is worth telling the user about (`changed examples/src/HelloWorld.els`)
  */
final class ProgressActivity private (val verb: String, subjectOf: () => String, val input: Boolean) {

  /** What is being done to. */
  lazy val subject: String = subjectOf()

  /** The same activity, marked as an input or not. */
  def asInput(input: Boolean = true): ProgressActivity = new ProgressActivity(verb, subjectOf, input)

  override def equals(other: Any): Boolean =
    other match {
      case that: ProgressActivity => verb == that.verb && subject == that.subject && input == that.input
      case _                      => false
    }

  override def hashCode(): Int = (verb, subject, input).hashCode()

  override def toString: String = s"ProgressActivity($verb, $subject, $input)"
}

object ProgressActivity {

  /** An activity whose `subject` is worked out when first read. */
  def apply(verb: String, subject: => String, input: Boolean = false): ProgressActivity =
    new ProgressActivity(verb, () => subject, input)
}
