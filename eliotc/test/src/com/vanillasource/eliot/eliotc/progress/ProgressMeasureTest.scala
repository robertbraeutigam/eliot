package com.vanillasource.eliot.eliotc.progress

import com.vanillasource.eliot.eliotc.progress.ProgressLineWriterTest.sgr
import com.vanillasource.eliot.eliotc.progress.ProgressMeasure.Quantity.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ProgressMeasureTest extends AnyFlatSpec with Matchers {

  "a size" should "be shown in bytes under a kilobyte" in {
    Bytes.show(1023) shouldBe "1023 B"
  }

  it should "be shown to three digits in kilobytes" in {
    Seq(2192L, 42240L, 421888L).map(Bytes.show) shouldBe Seq("2.14 KB", "41.3 KB", "412 KB")
  }

  it should "be shown in megabytes above a thousand kilobytes" in {
    Bytes.show(1363149L) shouldBe "1.30 MB"
  }

  it should "show a change with its sign" in {
    Seq(88L, -1229L, 0L).map(Bytes.showChange) shouldBe Seq("+88 B", "-1.20 KB", "+0 B")
  }

  "a count" should "be shown grouped" in {
    Count.showChange(1204) shouldBe "+1,204"
  }

  "a measure" should "show its limit and its change" in {
    ProgressMeasure("flash", 2192, Bytes, Some(262144)).show(Some(2180), ProgressStyle.undecorated) shouldBe
      "flash 2.14 KB / 256 KB (+12 B)"
  }

  it should "show a value over its limit as a failure" in {
    ProgressMeasure("ram", 64, Bytes, Some(32))
      .show(None, ProgressStyle(ProgressStyle.Colour.Ansi, unicode = true, timestamped = false)) shouldBe
      s"ram ${sgr(31)}64 B${sgr(0)} / 32 B"
  }
}
