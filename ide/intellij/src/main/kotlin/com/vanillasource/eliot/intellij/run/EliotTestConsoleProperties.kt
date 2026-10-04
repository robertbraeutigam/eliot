package com.vanillasource.eliot.intellij.run

import com.intellij.execution.Executor
import com.intellij.execution.testframework.sm.runner.SMTRunnerConsoleProperties

/**
 * What the platform's test runner needs to know about an Eliot test run: whose configuration it is and what to call the
 * framework. Everything else — the results tree, the pass/fail counts, re-run failed — is the platform's, driven by the
 * TeamCity service messages the test runner prints under `--format=teamcity` (`eliot.test.Report`).
 *
 * The messages nest a suite per module and a suite per subject around one test per case, so the tree reads
 * module → subject → `should` phrase, and a failed assertion that is a comparison carries `expected`/`actual`, which the
 * platform offers as a diff.
 */
class EliotTestConsoleProperties(configuration: EliotRunConfiguration, executor: Executor) :
  SMTRunnerConsoleProperties(configuration, FRAMEWORK_NAME, executor) {

  companion object {
    const val FRAMEWORK_NAME = "Eliot"
  }
}
