package com.vanillasource.eliot.intellij

import com.intellij.codeInsight.editorActions.enter.EnterHandlerDelegate
import com.intellij.codeInsight.editorActions.enter.EnterHandlerDelegateAdapter
import com.intellij.openapi.actionSystem.DataContext
import com.intellij.openapi.editor.Editor
import com.intellij.openapi.util.TextRange
import com.intellij.psi.PsiFile

/**
 * Continues block and doc comments when Enter is pressed inside one, mirroring IntelliJ's native Java/Scala
 * behaviour: pressing Enter on a doc-comment line inserts an aligned star-space prefix on the new line so the
 * user can keep typing the comment. (Comment markers are spelled out in prose below rather than written
 * literally — a slash-star sequence inside a KDoc opens a nested block comment and breaks the build.)
 *
 * This is a purely client-side editor feature, contributed through the `com.intellij.enterHandlerDelegate`
 * extension point — the same mechanism the JVM/Scala plugins use, and intentionally NOT the language server:
 * comment continuation is a typing gesture, not a semantic query, so it must be instant and work even while
 * the server is starting or the buffer does not type-check. Because `.els` files have no registered IntelliJ
 * `Language` (they are backed by the bundled TextMate grammar), the delegate is global and self-selects by
 * file extension.
 *
 * Behaviour, matching the IDE's Java doc-comment handler:
 * - After a comment-open (slash-star-star or slash-star) or a star-continuation line, the new line gets an
 *   aligned star-space prefix.
 * - When Enter splits an auto-closed empty doc comment (the star-slash close sits alone on the new line), the
 *   close is pushed to its own aligned line and the caret lands on a fresh star line between them.
 * - Nothing is inserted once the comment is closed (its star-slash reached) or outside any block comment; the
 *   gate is a real scan for an unterminated comment-open (ignoring line comments and string literals), so a
 *   stray star at the start of a code line never triggers it.
 */
class EliotCommentEnterHandler : EnterHandlerDelegateAdapter() {
  override fun postProcessEnter(file: PsiFile, editor: Editor, dataContext: DataContext): EnterHandlerDelegate.Result {
    if (!file.name.endsWith(".els")) return EnterHandlerDelegate.Result.Continue

    val document = editor.document
    val caretOffset = editor.caretModel.offset
    val caretLine = document.getLineNumber(caretOffset)
    if (caretLine == 0) return EnterHandlerDelegate.Result.Continue

    val prevStart = document.getLineStartOffset(caretLine - 1)
    val prevEnd = document.getLineEndOffset(caretLine - 1)
    // Gate on actually being inside a still-open block comment at the end of the previous line: this rules out
    // closed and single-line comments as well as star-starting code lines in one check.
    if (!insideBlockComment(document.immutableCharSequence, prevEnd)) return EnterHandlerDelegate.Result.Continue

    val prevLine = document.getText(TextRange(prevStart, prevEnd))
    val prevIndent = prevLine.takeWhile { it == ' ' || it == '\t' }
    // The star column: one space in from a comment-open (aligning under its first star), or the existing
    // star's own column on a continuation line.
    val starIndent = if (prevLine.trim().startsWith("/*")) "$prevIndent " else prevIndent
    val prefix = "$starIndent* "

    val lineStart = document.getLineStartOffset(caretLine)
    val lineEnd = document.getLineEndOffset(caretLine)

    if (document.getText(TextRange(lineStart, lineEnd)).trim() == "*/") {
      // Enter split an auto-closed comment: keep the close on its own aligned line below the new star line.
      document.replaceString(lineStart, lineEnd, "$prefix\n$starIndent*/")
    } else {
      // Replace whatever indentation the platform copied onto the new line with the aligned star-space prefix.
      document.replaceString(lineStart, caretOffset, prefix)
    }
    editor.caretModel.moveToOffset(lineStart + prefix.length)
    return EnterHandlerDelegate.Result.Stop
  }

  /**
   * Whether [end] sits inside an unterminated block comment in [text], scanning from the start while skipping
   * line comments and double-quoted string literals so their comment-open sequences never count.
   */
  private fun insideBlockComment(text: CharSequence, end: Int): Boolean {
    var i = 0
    var inBlock = false
    var inLine = false
    var inString = false
    while (i < end) {
      val c = text[i]
      val next = if (i + 1 < end) text[i + 1] else ' '
      when {
        inLine -> if (c == '\n') inLine = false
        inBlock -> if (c == '*' && next == '/') { inBlock = false; i++ }
        inString -> when {
          c == '\\' -> i++            // skip the escaped character
          c == '"' || c == '\n' -> inString = false
        }
        c == '/' && next == '/' -> { inLine = true; i++ }
        c == '/' && next == '*' -> { inBlock = true; i++ }
        c == '"' -> inString = true
      }
      i++
    }
    return inBlock
  }
}
