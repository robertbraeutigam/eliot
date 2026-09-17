package com.vanillasource.eliot.eliotc.progress

import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Path}

class ProgressProfileTest extends AsyncFlatSpec with AsyncIOSpec with Matchers {

  "the progress profile" should "read back the total it wrote" in {
    temporaryFile.flatMap(file => ProgressProfile.write(file, ProgressProfile(Some(27181))) >> ProgressProfile.read(file))
      .asserting(_ shouldBe ProgressProfile(Some(27181)))
  }

  it should "be empty when there is no file" in {
    temporaryFile.flatMap(file => ProgressProfile.read(file)).asserting(_ shouldBe ProgressProfile.empty)
  }

  it should "create the target directory it is written to" in {
    temporaryFile
      .map(_.resolveSibling("target").resolve("profile"))
      .flatMap(file => ProgressProfile.write(file, ProgressProfile(Some(3))) >> ProgressProfile.read(file))
      .asserting(_ shouldBe ProgressProfile(Some(3)))
  }

  it should "ignore lines it does not know" in {
    ProgressProfile.parse("cold 12 3400\ntotal 27181\n") shouldBe ProgressProfile(Some(27181))
  }

  it should "have no total when the total does not parse" in {
    ProgressProfile.parse("total many\n") shouldBe ProgressProfile.empty
  }

  it should "have no total when the total is negative" in {
    ProgressProfile.parse("total -1\n") shouldBe ProgressProfile.empty
  }

  it should "render nothing when it has no total" in {
    ProgressProfile.empty.render shouldBe ""
  }

  it should "name its file by the configuration's fingerprint" in {
    ProgressProfile.fileIn(Path.of("target"), "ab:cd0123456789abcdef99") shouldBe
      Path.of("target", ".eliot-progress-abcd0123456789ab")
  }

  /** A path in a fresh directory, where no file exists yet. */
  private def temporaryFile: IO[Path] =
    IO.blocking(Files.createTempDirectory("eliot-progress").resolve("profile"))
}
