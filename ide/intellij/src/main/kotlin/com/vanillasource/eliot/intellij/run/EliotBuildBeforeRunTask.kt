package com.vanillasource.eliot.intellij.run

import com.intellij.execution.BeforeRunTask

/**
 * The "build the executable jar" step attached to an [EliotRunConfiguration]. Carries no state of its own;
 * the actual compile is performed by [EliotBuildBeforeRunTaskProvider.executeTask] from the configuration's
 * settings. Enabled by default so attaching it is enough to make the build run.
 */
class EliotBuildBeforeRunTask : BeforeRunTask<EliotBuildBeforeRunTask>(EliotBuildBeforeRunTaskProvider.ID) {
  init {
    isEnabled = true
  }
}
