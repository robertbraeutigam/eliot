package com.vanillasource.eliot.eliotc.compiler

import cats.effect.ExitCode
import cats.effect.testing.scalatest.AsyncIOSpec
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

/** What the exit code says about a command line the compiler never ran.
  *
  * A build tool runs the compiler once per `compiler` line of a closure and gates on the code, so a line this compiler
  * does not understand — a flag added after the tag the line was written against, say — has to read as a failure.
  * Exiting 0 there made a build that compiled nothing report success.
  */
class CommandLineExitCodeTest extends AsyncFlatSpec with AsyncIOSpec with Matchers {

  "the compiler" should "fail on a command line it refuses" in {
    Compiler.runCompiler(List("--no-such-option")).asserting(_ shouldBe ExitCode.Error)
  }

  it should "fail on a command line naming no target at all" in {
    Compiler.runCompiler(List.empty).asserting(_ shouldBe ExitCode.Error)
  }

  it should "succeed on the help it was asked for" in {
    Compiler.runCompiler(List("--help")).asserting(_ shouldBe ExitCode.Success)
  }
}
