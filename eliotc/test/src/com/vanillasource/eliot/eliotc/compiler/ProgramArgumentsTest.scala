package com.vanillasource.eliot.eliotc.compiler

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The `--` that hands the rest of the command line to the program ([[Compiler.splitProgramArguments]]): the compiler
  * parses everything before the first one, the program receives everything after it, untouched.
  */
class ProgramArgumentsTest extends AnyFlatSpec with Matchers {

  "splitting the command line" should "leave a line without a separator all the compiler's" in {
    Compiler.splitProgramArguments(List("run", "-m", "Main", "src/")) shouldBe ((List("run", "-m", "Main", "src/"), Nil))
  }

  it should "hand everything after the first separator to the program" in {
    Compiler.splitProgramArguments(List("run", "-m", "Main", "--", "a", "b")) shouldBe ((List("run", "-m", "Main"), List("a", "b")))
  }

  it should "pass a second separator through to the program" in {
    Compiler.splitProgramArguments(List("run", "--", "a", "--", "b")) shouldBe ((List("run"), List("a", "--", "b")))
  }

  it should "pass options after the separator through to the program, not the compiler" in {
    Compiler.splitProgramArguments(List("run", "--", "--format=plain", "-o", "x")) shouldBe ((List("run"), List("--format=plain", "-o", "x")))
  }

  it should "give a trailing separator no arguments" in {
    Compiler.splitProgramArguments(List("run", "--")) shouldBe ((List("run"), Nil))
  }

  it should "leave an empty line empty" in {
    Compiler.splitProgramArguments(Nil) shouldBe ((Nil, Nil))
  }
}
