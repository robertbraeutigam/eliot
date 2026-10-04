package com.vanillasource.eliot.intellij.language

import com.intellij.lang.Commenter

/** Comment/uncomment (Ctrl+/, Ctrl+Shift+/) with Eliot's `//` and `/* … */`. */
class EliotCommenter : Commenter {
  override fun getLineCommentPrefix(): String = "//"

  override fun getBlockCommentPrefix(): String = "/*"

  override fun getBlockCommentSuffix(): String = "*/"

  override fun getCommentedBlockCommentPrefix(): String? = null

  override fun getCommentedBlockCommentSuffix(): String? = null
}
