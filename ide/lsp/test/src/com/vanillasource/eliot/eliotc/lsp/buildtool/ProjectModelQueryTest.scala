package com.vanillasource.eliot.eliotc.lsp.buildtool

import cats.effect.{IO, Resource}
import cats.effect.testing.scalatest.AsyncIOSpec
import com.vanillasource.eliot.eliotc.lsp.buildtool.ProjectModel.Unresolved
import org.scalatest.EitherValues
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.attribute.PosixFilePermissions
import java.nio.file.{Files, Path}
import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*

/** Runs a stand-in `eliotw` — a shell script answering the way the build tool does — so what is checked is the process
  * handling: the command line, the working directory, exit codes and the timeout.
  */
class ProjectModelQueryTest extends AsyncFlatSpec with AsyncIOSpec with Matchers with EitherValues {

  "the project-model query" should "read the model the wrapper prints, run in the project's directory" in {
    project("""[ "$1" = --project-model ] && [ -f eliot.pkg ] && echo '{"packages": [{"name": "t", "problem": "p"}]}'""")
      .use(ProjectModelQuery.query)
      .asserting(_ shouldBe Right(ProjectModel(Seq(Unresolved("t", "p")))))
  }

  it should "report a wrapper that exits non-zero, with what it printed" in {
    project("echo 'usage: eliot <package>'; exit 1")
      .use(ProjectModelQuery.query)
      .asserting(_.left.value should endWith("eliotw --project-model' exited with 1: usage: eliot <package>"))
  }

  it should "report a wrapper that prints something other than a model" in {
    project("echo hello").use(ProjectModelQuery.query).asserting(_.left.value should startWith("not a project model"))
  }

  it should "give up on a wrapper that does not answer in time" in {
    project("sleep 30")
      .use(directory => ProjectModelQuery.run(ProjectModelQuery.command(directory), directory, 200.millis))
      .asserting(_.left.value should endWith("did not answer within 200 milliseconds"))
  }

  it should "report a command that cannot be started" in {
    project("true")
      .use(directory => ProjectModelQuery.run(Seq(directory.resolve("nosuch").toString), directory, 1.second))
      .asserting(_.left.value should include("could not be run"))
  }

  it should "run a wrapper that lost its executable bit through sh" in {
    project("echo '{\"packages\": []}'", executable = false)
      .use(ProjectModelQuery.query)
      .asserting(_ shouldBe Right(ProjectModel(Seq.empty)))
  }

  it should "take a directory with both a descriptor and a wrapper for a project" in {
    project("true").use(directory => IO(ProjectModelQuery.isProject(directory))).asserting(_ shouldBe true)
  }

  it should "not take a directory without a wrapper for a project" in {
    project("true")
      .use(directory => IO.blocking(Files.delete(directory.resolve("eliotw"))) >> IO(ProjectModelQuery.isProject(directory)))
      .asserting(_ shouldBe false)
  }

  private def project(script: String, executable: Boolean = true): Resource[IO, Path] =
    Resource.make(IO.blocking {
      val directory = Files.createTempDirectory("eliot-lsp-query")
      val wrapper   = directory.resolve("eliotw")
      Files.writeString(directory.resolve("eliot.pkg"), "launcher v0.6\n")
      Files.writeString(wrapper, s"#!/bin/sh\n$script\n")
      if (executable) Files.setPosixFilePermissions(wrapper, PosixFilePermissions.fromString("rwxr-xr-x"))
      directory
    })(directory =>
      IO.blocking(Files.walk(directory).sorted(java.util.Comparator.reverseOrder()).iterator().asScala.foreach(Files.delete))
    )
}
