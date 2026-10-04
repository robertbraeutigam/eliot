package com.vanillasource.eliot.intellij.run

import com.intellij.execution.lineMarker.RunLineMarkerContributor
import com.intellij.icons.AllIcons
import com.intellij.psi.PsiElement
import com.vanillasource.eliot.intellij.language.EliotLanguage

/**
 * The green ▶ in the gutter next to a `main` and a suite, as for a Java `main` or test class. Its menu is the platform's
 * own executor actions (Run, Debug, Modify Run Configuration…), which build the configuration through
 * [EliotRunConfigurationProducer] from the element the icon sits on.
 *
 * The icon is anchored to the leaf the server's lens starts in — a word of [EliotLanguage]'s flat PSI, which is why
 * `.els` has a language of its own at all: a TextMate file is one leaf, with nothing per line to anchor to.
 */
class EliotRunLineMarkerContributor : RunLineMarkerContributor() {
  override fun getInfo(element: PsiElement): Info? {
    if (element.firstChild != null) return null
    val target = EliotRunTargets.startingIn(element) ?: return null
    return withExecutorActions(
      when (target.kind) {
        EliotRunTarget.Kind.MAIN -> AllIcons.RunConfigurations.TestState.Run
        EliotRunTarget.Kind.TESTS -> AllIcons.RunConfigurations.TestState.Run_run
      }
    )
  }
}
