package com.vanillasource.eliot.intellij.language

import com.intellij.extapi.psi.PsiFileBase
import com.intellij.openapi.fileTypes.FileType
import com.intellij.psi.FileViewProvider

/** The PSI root of an `.els` file: a flat sequence of word and whitespace leaves (see [EliotParserDefinition]). */
class EliotFile(viewProvider: FileViewProvider) : PsiFileBase(viewProvider, EliotLanguage) {
  override fun getFileType(): FileType = EliotFileType

  override fun toString(): String = "Eliot file"
}
