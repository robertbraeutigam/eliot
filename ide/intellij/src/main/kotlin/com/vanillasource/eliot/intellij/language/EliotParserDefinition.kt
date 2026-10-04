package com.vanillasource.eliot.intellij.language

import com.intellij.extapi.psi.ASTWrapperPsiElement
import com.intellij.lang.ASTNode
import com.intellij.lang.ParserDefinition
import com.intellij.lang.PsiParser
import com.intellij.lexer.Lexer
import com.intellij.openapi.project.Project
import com.intellij.psi.FileViewProvider
import com.intellij.psi.PsiElement
import com.intellij.psi.PsiFile
import com.intellij.psi.tree.IElementType
import com.intellij.psi.tree.IFileElementType
import com.intellij.psi.tree.TokenSet

/**
 * The PSI of an `.els` file: the file, directly holding the [EliotLexer]'s leaves. There are no composite nodes — the
 * plugin reads no syntax, and a deeper tree would be a second parser of Eliot beside the compiler's.
 */
class EliotParserDefinition : ParserDefinition {
  override fun createLexer(project: Project?): Lexer = EliotLexer()

  override fun createParser(project: Project?): PsiParser = PsiParser { root, builder ->
    val file = builder.mark()
    while (!builder.eof()) builder.advanceLexer()
    file.done(root)
    builder.treeBuilt
  }

  override fun getFileNodeType(): IFileElementType = FILE

  override fun getCommentTokens(): TokenSet = TokenSet.EMPTY

  override fun getStringLiteralElements(): TokenSet = TokenSet.EMPTY

  override fun createElement(node: ASTNode): PsiElement = ASTWrapperPsiElement(node)

  override fun createFile(viewProvider: FileViewProvider): PsiFile = EliotFile(viewProvider)

  companion object {
    val FILE = IFileElementType(EliotLanguage)

    /** A run of non-whitespace characters. */
    val WORD = IElementType("WORD", EliotLanguage)
  }
}
