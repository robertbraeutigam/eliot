package com.vanillasource.eliot.eliotc.apidoc.render

/** A lightweight, dependency-free syntax highlighter for Eliot source, used for both rendered signatures and the
  * `eliot` code blocks embedded in documentation. It mirrors the lexical conventions of the `ide/textmate` grammar: it
  * leans on Eliot's case rule (an upper-case-initial identifier is a type/constructor, a lower-case-initial one a
  * value/function) rather than any type information, so it works on arbitrary fragments.
  *
  * It returns HTML with `<span class="...">` wrappers (classes `kw`, `ty`, `fn`, `num`, `str`, `cm`, `op`); everything
  * is HTML-escaped. Punctuation and whitespace are emitted without a wrapper.
  */
object EliotHighlighter {
  private val keywords = Set(
    "import", "data", "def", "ability", "implement", "match", "case", "type", "infix", "prefix", "postfix", "val",
    "private", "auto", "left", "right", "none", "at", "above", "below"
  )

  private def isOperatorChar(c: Char): Boolean = "!#$%&*+./<=>?@\\^|-~;:".contains(c)

  def highlight(code: String): String = {
    val out = new StringBuilder
    val n   = code.length
    var i   = 0

    def span(cssClass: String, text: String): Unit = {
      out.append(s"""<span class="$cssClass">""").append(escape(text)).append("</span>")
      ()
    }

    while (i < n) {
      val c = code.charAt(i)
      if (c == '/' && code.startsWith("//", i)) {
        val end = { val nl = code.indexOf('\n', i); if (nl < 0) n else nl }
        span("cm", code.substring(i, end)); i = end
      } else if (c == '/' && code.startsWith("/*", i)) {
        val close = code.indexOf("*/", i + 2)
        val end   = if (close < 0) n else close + 2
        span("cm", code.substring(i, end)); i = end
      } else if (c == '"') {
        var j = i + 1
        while (j < n && code.charAt(j) != '"') j = if (code.charAt(j) == '\\') j + 2 else j + 1
        val end = math.min(j + 1, n)
        span("str", code.substring(i, end)); i = end
      } else if (c.isLetter) {
        var j = i
        while (j < n && code.charAt(j).isLetterOrDigit) j += 1
        val word = code.substring(i, j)
        span(if (keywords(word)) "kw" else if (word.head.isUpper) "ty" else "fn", word); i = j
      } else if (c.isDigit || (c == '-' && i + 1 < n && code.charAt(i + 1).isDigit)) {
        var j = i + 1
        while (j < n && code.charAt(j).isDigit) j += 1
        span("num", code.substring(i, j)); i = j
      } else if (isOperatorChar(c)) {
        var j = i
        while (j < n && isOperatorChar(code.charAt(j))) j += 1
        span("op", code.substring(i, j)); i = j
      } else {
        out.append(escape(c.toString)); i += 1
      }
    }
    out.toString
  }

  def escape(text: String): String =
    text.flatMap {
      case '&' => "&amp;"
      case '<' => "&lt;"
      case '>' => "&gt;"
      case '"' => "&quot;"
      case c   => c.toString
    }
}
