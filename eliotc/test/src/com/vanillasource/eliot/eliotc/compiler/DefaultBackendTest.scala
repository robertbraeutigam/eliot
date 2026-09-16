package com.vanillasource.eliot.eliotc.compiler

import cats.data.StateT
import cats.effect.IO
import com.vanillasource.eliot.eliotc.plugin.{CompilerPlugin, Configuration}
import com.vanillasource.eliot.eliotc.processor.CompilerProcessor
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The rule that lets a command line leave its backend out ([[Compiler.withDefaultBackend]]): a line starting with a
  * mode word belongs to the one backend accepting that mode, and every other line is left for the parser to judge.
  */
class DefaultBackendTest extends AnyFlatSpec with Matchers {
  import DefaultBackendTest.*

  "the default backend" should "be filled in for a mode only one backend accepts" in {
    Compiler.withDefaultBackend(List("run", "-m", "Main"), Seq(frontEnd, jvm)) shouldBe List("jvm", "run", "-m", "Main")
  }

  it should "leave a line that names its backend alone" in {
    Compiler.withDefaultBackend(List("jvm", "run", "-m", "Main"), Seq(jvm)) shouldBe List("jvm", "run", "-m", "Main")
  }

  it should "leave a line alone when two backends accept its mode" in {
    Compiler.withDefaultBackend(List("run", "-m", "Main"), Seq(jvm, chip)) shouldBe List("run", "-m", "Main")
  }

  it should "leave a line alone when no backend accepts its first word" in {
    Compiler.withDefaultBackend(List("apidoc", "src"), Seq(jvm, chip)) shouldBe List("apidoc", "src")
  }

  it should "pick the backend by mode, not by being the only one present" in {
    Compiler.withDefaultBackend(List("flash", "-m", "Main"), Seq(jvm, chip)) shouldBe List("mcu", "flash", "-m", "Main")
  }

  it should "leave an empty line empty" in {
    Compiler.withDefaultBackend(Nil, Seq(jvm)) shouldBe Nil
  }
}

object DefaultBackendTest {
  private class Plugin(word: Option[String], modes: Seq[String]) extends CompilerPlugin {
    override def backendWord: Option[String]                                                       = word
    override def backendModes: Seq[String]                                                         = modes
    override def initialize(configuration: Configuration): StateT[IO, CompilerProcessor, Unit] = StateT.empty
  }

  private val frontEnd = Plugin(None, Seq.empty)
  private val jvm      = Plugin(Some("jvm"), Seq("exe-jar", "run"))
  private val chip     = Plugin(Some("mcu"), Seq("run", "flash"))
}
