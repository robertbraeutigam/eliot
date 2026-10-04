package com.vanillasource.eliot.intellij.run

import com.intellij.execution.lineMarker.RunLineMarkerContributor
import com.intellij.icons.AllIcons
import com.intellij.psi.PsiElement

/**
 * The green ▶ in the gutter next to a `main` and a suite, as for a Java `main` or test class. Its menu is the platform's
 * own executor actions (Run, Debug, Modify Run Configuration…), which build the configuration through
 * [EliotRunConfigurationProducer] from the element the icon sits on.
 *
 * `.els` files have no language of their own — the TextMate bundle owns them — so this is registered for the `textmate`
 * language (`eliot-textmate.xml`) and answers only in `.els` files ([EliotRunTargets.of] checks). A TextMate file's PSI
 * is one leaf per lexer token; the icon is anchored to the leaf the server's lens starts in.
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
