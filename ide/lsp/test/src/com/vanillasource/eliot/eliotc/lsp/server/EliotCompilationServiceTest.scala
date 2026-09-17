package com.vanillasource.eliot.eliotc.lsp.server

import cats.effect.unsafe.IORuntime
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Path}

class EliotCompilationServiceTest extends AnyFlatSpec with Matchers {
  private val folder  = Files.createTempDirectory("eliot-lsp-service")
  private val service = new EliotCompilationService(IORuntime.global)
  service.startWorkspace(Seq(folder))

  private def changed(relative: String): Boolean = service.isPlanInput(folder.resolve(relative).toUri)

  "the compilation service" should "replan when a workspace folder's descriptor changes" in {
    changed("eliot.pkg") shouldBe true
  }

  it should "replan when a workspace folder's lockfile changes" in {
    changed("eliot.lock") shouldBe true
  }

  it should "not replan for a descriptor that is not a workspace folder's own" in {
    changed("vendor/eliot.pkg") shouldBe false
  }

  it should "not replan for a source file" in {
    changed("src/Main.els") shouldBe false
  }

  it should "not replan for a URI that names no file" in {
    service.isPlanInput(java.net.URI.create("lsp-main:lspmain/Main.els")) shouldBe false
  }

  it should "not replan for a descriptor outside every workspace folder" in {
    service.isPlanInput(Path.of("/elsewhere/eliot.pkg").toUri) shouldBe false
  }
}
