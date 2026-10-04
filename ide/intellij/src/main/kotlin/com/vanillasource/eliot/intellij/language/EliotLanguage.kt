package com.vanillasource.eliot.intellij.language

import com.intellij.lang.Language

/**
 * The IntelliJ language of `.els` files. It exists so a file has PSI with one leaf per word ([EliotParserDefinition]),
 * which is what IntelliJ's per-line features anchor to — a gutter run icon most of all. A TextMate file cannot carry
 * them: TextMate's parser builds the whole file as a single leaf. Highlighting is still the TextMate grammar's,
 * registered for this language in `eliot-textmate.xml`; everything semantic is the language server's.
 */
object EliotLanguage : Language("Eliot") {
  private fun readResolve(): Any = EliotLanguage
}
