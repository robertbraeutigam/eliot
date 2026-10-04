package com.vanillasource.eliot.intellij.language

import com.intellij.openapi.fileTypes.SyntaxHighlighterFactory
import com.intellij.psi.util.PsiTreeUtil
import com.intellij.testFramework.fixtures.BasePlatformTestCase

class EliotLanguageTest : BasePlatformTestCase() {
  fun testElsFileIsEliot() {
    assertEquals(EliotFileType, myFixture.configureByText("Main.els", "def main: Unit").fileType)
  }

  fun testLeavesAreWordsAndWhitespace() {
    val file = myFixture.configureByText("Main.els", "def main: {Console} Unit\n  = x")
    assertEquals(listOf("def", " ", "main:", " ", "{Console}", " ", "Unit", "\n  ", "=", " ", "x"), PsiTreeUtil.collectElements(file) { it.firstChild == null }.map { it.text })
  }

  fun testColouringIsTextMates() {
    val file = myFixture.configureByText("Main.els", "def main: Unit")
    assertTrue(SyntaxHighlighterFactory.getSyntaxHighlighter(EliotLanguage, project, file.virtualFile)!!.javaClass.name.startsWith("org.jetbrains.plugins.textmate."))
  }

  fun testTextMateGrammarIsFoundForEliotFiles() {
    val file = myFixture.configureByText("Main.els", "def main: Unit")
    val lexer = SyntaxHighlighterFactory.getSyntaxHighlighter(EliotLanguage, project, file.virtualFile)!!.highlightingLexer
    lexer.start("def main: Unit")
    val types = generateSequence { lexer.tokenType?.also { lexer.advance() } }.toSet()
    assertTrue("one token type means no grammar was found: $types", types.size > 1)
  }
}
