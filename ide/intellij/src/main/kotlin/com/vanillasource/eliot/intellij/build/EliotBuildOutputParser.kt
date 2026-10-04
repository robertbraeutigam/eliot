package com.vanillasource.eliot.intellij.build

/**
 * Reads the Eliot compiler's output, a line at a time, into what the Build tool window shows besides the raw text: the
 * compiler's errors, each with its position, and the `--progress` lines.
 *
 * An error is a header line followed by its source snippet and description, every one of them starting with a gutter
 * (`  |`, ` 12 |`):
 * ```
 * src/Main.els:error:3:9:Name not defined.
 *   |
 * 3 | def main: Unit = foo
 *   |                  ^^^
 * ```
 * so an error is complete at the first line that is not part of it, or at [finish]. The compiler colours both, so every
 * line is read with its ANSI escapes removed. A progress line is the log form `--progress` writes when its output is
 * not a terminal — `12:00:01 [ 1,327 facts ] parsing src/Main.els  1.8s`, or `[  412/1,327]` once the run has a total
 * to count against — and the lines it opens and ends a run with (`eliot · jvm exe-jar · Main`, `ok …`, `failed …`) are
 * progress too, without a count.
 *
 * The format read here is the compiler's human-readable one, not a promised interface: the plugin only ever runs the
 * compiler it bundles, built from the same repository, so the two cannot drift apart unnoticed by its tests.
 *
 * The compiler can report one error more than once; a repeat of an error already read is dropped.
 */
class EliotBuildOutputParser {
  private var pending: Problem? = null
  private val reported = mutableSetOf<Problem>()

  /** What [line] completes: the error it ends, if any, and the error or progress it is. */
  fun accept(line: String): List<Event> {
    val plain = ANSI.replace(line, "")
    if (pending != null && GUTTER.matches(plain)) {
      pending = pending!!.copy(detail = pending!!.detail + "\n" + plain)
      return emptyList()
    }
    val completed = flush()
    val header = HEADER.matchEntire(plain)
    if (header != null) {
      val (file, row, column, message) = header.destructured
      pending = Problem(file, row.toInt(), column.toInt(), message.trim(), plain)
      return completed
    }
    return completed + listOfNotNull(progressOf(plain))
  }

  /** The error still being read when the output ends. */
  fun finish(): List<Event> = flush()

  private fun flush(): List<Event> {
    val problem = pending ?: return emptyList()
    pending = null
    return if (reported.add(problem)) listOf(problem) else emptyList()
  }

  private fun progressOf(plain: String): Progress? {
    val text = TIME.matchEntire(plain)?.groupValues?.get(1)?.trim() ?: return null
    val counter = COUNTER.matchEntire(text) ?: return Progress(text, null, null)
    val (done, total, rest) = counter.destructured
    return Progress(rest.trim(), number(done), total.takeIf { it.isNotEmpty() }?.let(::number))
  }

  private fun number(digits: String): Long = digits.replace(",", "").toLong()

  /** Something the Build window shows beyond the raw output. */
  sealed interface Event

  /**
   * A compiler error at [line]:[column] (both 1-based) of [file], which is a path — or `eliotc` for an error of the
   * compiler itself, which has no position. [detail] is the whole report: header, snippet and description.
   */
  data class Problem(val file: String, val line: Int, val column: Int, val message: String, val detail: String) : Event

  /** What the run is doing, with the facts [done] so far out of [total] when the line counts them. */
  data class Progress(val text: String, val done: Long?, val total: Long?) : Event

  private companion object {
    val ANSI = Regex("\u001B\\[[0-9;]*[A-Za-z]")
    val HEADER = Regex("(.*):error:(\\d+):(\\d+):(.*)")
    val GUTTER = Regex("\\s*\\d*\\s*\\|.*")
    val TIME = Regex("\\d\\d:\\d\\d:\\d\\d (.*)")
    val COUNTER = Regex("\\[\\s*([\\d,]+)(?:/([\\d,]+))?(?: facts)?\\s*](.*)")
  }
}
