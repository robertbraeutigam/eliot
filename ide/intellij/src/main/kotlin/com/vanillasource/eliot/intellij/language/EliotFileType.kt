package com.vanillasource.eliot.intellij.language

import com.intellij.icons.AllIcons
import com.intellij.openapi.fileTypes.LanguageFileType
import javax.swing.Icon

/** The `.els` file type, of [EliotLanguage]. Registered with `fieldName="INSTANCE"`, the field an `object` compiles to. */
object EliotFileType : LanguageFileType(EliotLanguage) {
  override fun getName(): String = "Eliot"

  override fun getDescription(): String = "Eliot source"

  override fun getDefaultExtension(): String = "els"

  override fun getIcon(): Icon = AllIcons.FileTypes.Text
}
