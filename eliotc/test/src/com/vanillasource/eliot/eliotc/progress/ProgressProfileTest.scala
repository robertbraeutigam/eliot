package com.vanillasource.eliot.eliotc.progress

import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import com.vanillasource.eliot.eliotc.progress.ProgressRunClass.*
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Path}

class ProgressProfileTest extends AsyncFlatSpec with AsyncIOSpec with Matchers {

  "the progress profile" should "read back the total it wrote" in {
    temporaryFile.flatMap(file => ProgressProfile.write(file, ProgressProfile(Some(27181))) >> ProgressProfile.read(file))
      .asserting(_ shouldBe ProgressProfile(Some(27181)))
  }

  it should "read back the histories it wrote" in {
    temporaryFile.flatMap(file => ProgressProfile.write(file, withHistory) >> ProgressProfile.read(file))
      .asserting(_ shouldBe withHistory)
  }

  it should "start the history of a class with its first run" in {
    ProgressProfile.empty.including(3, Unchanged, withHistory.runs(Unchanged)) shouldBe
      withHistory.copy(runs = withHistory.runs - Cold)
  }

  it should "ignore a history line that does not parse" in {
    ProgressProfile.parse("total 3\ncold type Key 1\nhot facts 2\ncold phase Nowhere 3\ncold facts many\n") shouldBe
      ProgressProfile(Some(3))
  }

  it should "read back the measures it wrote" in {
    temporaryFile
      .flatMap(file => ProgressProfile.write(file, withMeasures) >> ProgressProfile.read(file))
      .asserting(_ shouldBe withMeasures)
  }

  it should "keep only the measures of the latest run" in {
    withMeasures.including(3, Cold, ProgressHistory.empty, Seq(ProgressMeasure("ram", 32, ProgressMeasure.Quantity.Bytes)))
      .measures shouldBe Map("ram" -> 32)
  }

  it should "ignore a measure with no name or no value" in {
    ProgressProfile.parse("measure 12\nmeasure many HelloWorld.jar\nmeasure -1 x\n") shouldBe ProgressProfile.empty
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

  private val withHistory = ProgressProfile(
    Some(3),
    Map(
      Cold      -> ProgressHistory(3, Map(ProgressPhase.SavingCache -> 5e8), Map("parse$Key" -> ProgressCost(2.5, 7e6))),
      Unchanged -> ProgressHistory(3, Map(ProgressPhase.LoadingCache -> 1e8), Map.empty)
    )
  )

  private val withMeasures = ProgressProfile(Some(3), measures = Map("Hello World.jar" -> 421888, "flash" -> 2192))

  /** A path in a fresh directory, where no file exists yet. */
  private def temporaryFile: IO[Path] =
    IO.blocking(Files.createTempDirectory("eliot-progress").resolve("profile"))
}
