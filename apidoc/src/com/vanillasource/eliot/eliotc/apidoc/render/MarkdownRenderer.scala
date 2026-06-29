package com.vanillasource.eliot.eliotc.apidoc.render

import scala.util.matching.Regex

/** A small CommonMark-subset renderer for documentation bodies: paragraphs, ATX headings (`#`..`####`), unordered
  * lists (`-`/`*`), fenced code blocks, and the inline run of inline-code, `**bold**`, `*italic*`, and `[text](url)`
  * links. It is intentionally not a full CommonMark implementation — just the slice documentation actually uses.
  *
  * Fenced code blocks tagged `eliot` (or untagged) are syntax-highlighted with [[EliotHighlighter]]; any other language
  * tag is emitted escaped but unhighlighted. All text is HTML-escaped before inline formatting is applied; inline-code
  * segments are emitted verbatim (escaped) and never touched by the bold/italic/link rules.
  */
object MarkdownRenderer {
  private val heading  = "^(#{1,6})\\s+(.*)$".r
  private val listItem = "^[-*]\\s+(.*)$".r
  private val fence    = "^```\\s*([A-Za-z0-9_-]*)\\s*$".r
  private val link     = "\\[([^\\]]+)\\]\\(([^)]+)\\)".r
  private val bold     = "\\*\\*([^*]+)\\*\\*".r
  private val italic   = "\\*([^*]+)\\*".r

  def render(markdown: String): String = {
    val out       = new StringBuilder
    val lines     = markdown.split("\n", -1).toVector
    val paragraph = collection.mutable.ArrayBuffer.empty[String]
    var i         = 0

    def flushParagraph(): Unit =
      if (paragraph.nonEmpty) {
        out.append("<p>").append(renderInline(paragraph.mkString(" "))).append("</p>\n")
        paragraph.clear()
      }

    while (i < lines.length) {
      lines(i) match {
        case fence(lang)               =>
          flushParagraph()
          val code = collection.mutable.ArrayBuffer.empty[String]
          i += 1
          while (i < lines.length && !lines(i).trim.startsWith("```")) { code += lines(i); i += 1 }
          out.append(codeBlock(lang, code.mkString("\n")))
        case heading(hashes, text)     =>
          flushParagraph()
          val level = math.min(hashes.length + 3, 6) // page uses h1/h2; doc headings start at h4
          out.append(s"<h$level>").append(renderInline(text)).append(s"</h$level>\n")
        case listItem(_)               =>
          flushParagraph()
          out.append("<ul>\n")
          while (i < lines.length && listItem.matches(lines(i))) {
            val listItem(item) = lines(i): @unchecked
            out.append("<li>").append(renderInline(item)).append("</li>\n")
            i += 1
          }
          out.append("</ul>\n")
          i -= 1 // compensate for the trailing increment
        case line if line.trim.isEmpty => flushParagraph()
        case line                      => paragraph += line.trim
      }
      i += 1
    }
    flushParagraph()
    out.toString
  }

  private def codeBlock(lang: String, code: String): String = {
    val rendered = if (lang.isEmpty || lang == "eliot") EliotHighlighter.highlight(code) else EliotHighlighter.escape(code)
    s"""<pre class="code"><code>$rendered</code></pre>\n"""
  }

  /** Inline formatting. The text is split into alternating non-code / inline-code segments on backtick pairs; only the
    * non-code segments get the bold/italic/link rules, so code is never mangled. Everything is escaped first.
    */
  private def renderInline(text: String): String = {
    val escaped = EliotHighlighter.escape(text)
    val out     = new StringBuilder
    var i       = 0
    while (i < escaped.length) {
      val tick = escaped.indexOf('`', i)
      if (tick < 0) { out.append(formatNonCode(escaped.substring(i))); i = escaped.length }
      else {
        out.append(formatNonCode(escaped.substring(i, tick)))
        val close = escaped.indexOf('`', tick + 1)
        if (close < 0) { out.append(escaped.substring(tick)); i = escaped.length }
        else { out.append("<code>").append(escaped.substring(tick + 1, close)).append("</code>"); i = close + 1 }
      }
    }
    out.toString
  }

  private def formatNonCode(segment: String): String = {
    var s = segment
    s = link.replaceAllIn(s, m => Regex.quoteReplacement(s"""<a href="${m.group(2)}">${m.group(1)}</a>"""))
    s = bold.replaceAllIn(s, m => Regex.quoteReplacement(s"<strong>${m.group(1)}</strong>"))
    s = italic.replaceAllIn(s, m => Regex.quoteReplacement(s"<em>${m.group(1)}</em>"))
    s
  }
}
